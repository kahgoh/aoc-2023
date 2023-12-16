with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Maze is
   type Dimension is record
      Width : Integer := 0;
      Height : Integer := 0;
   end record;

   type Maze_Symbols is array (Integer range <>, Integer range <>)
      of Character;

   type Maze_Path is array (Integer range <>, Integer range <>)
      of Integer;

   type Candidate_Symbols is array (Integer range <>) of Character;

   Connect_North : constant Candidate_Symbols := ('|', '7', 'F', 'S');
   Connect_South : constant Candidate_Symbols := ('|', 'J', 'L', 'S');
   Connect_West : constant Candidate_Symbols := ('-', 'L', 'F', 'S');
   Connect_East : constant Candidate_Symbols := ('-', 'J', '7', 'S');
   Path_Symbols : constant Candidate_Symbols := ('|', '-', 'L', 'J',
      '7', 'F', 'S');

   type Point is record
      Row : Integer;
      Column : Integer;
   end record;

   package Visit_Queue is
      new Ada.Containers.Doubly_Linked_Lists (Element_Type => Point);

   --  Check if an array contains a specific element
   function Contains (Container : Candidate_Symbols;
      Elem : Character) return Boolean
   is
   begin
      for Index in Container'Range loop
         if Container (Index) = Elem then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --  Prints the maze to stdout
   procedure Print_Maze (Maze : Maze_Symbols)
   is
   begin
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            Ada.Text_IO.Put (Maze (Row, Col));
         end loop;
         Ada.Text_IO.Put_Line ("");
      end loop;
   end Print_Maze;

   --  Finds the dimensions of the map.
   function Get_Dimension (File_Name : String) return Dimension
   is
      File_Handle : Ada.Text_IO.File_Type;
      Width : Integer := 0;
      Height : Integer := 0;
   begin
      Ada.Text_IO.Open (File => File_Handle,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name);

      declare
         Line : String := Ada.Text_IO.Get_Line (File_Handle);
      begin
         Width := Line'Length;
         Height := 1;

         Ada.Text_IO.Put_Line ("Width: " & Integer'Image (Width));
         while not Ada.Text_IO.End_Of_File (File => File_Handle) loop
            Height := Height + 1;
            Line := Ada.Text_IO.Get_Line (File_Handle);
         end loop;
         Ada.Text_IO.Put_Line ("Height: " & Integer'Image (Height));
         Ada.Text_IO.Close (File_Handle);

         return (Width, Height);
      end;
   end Get_Dimension;

   --  Reads in the raw map directly from the file.
   function Read_Map (File_Name : String) return Maze_Symbols
   is
      File_Handle : Ada.Text_IO.File_Type;

      Dimensions : constant Dimension := Get_Dimension (File_Name);
      Map : Maze_Symbols (1 .. Dimensions.Height,
         1 .. Dimensions.Width);
      Row : Integer := 0;
   begin
      Ada.Text_IO.Open (File => File_Handle,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name);

      --  Read in the map symbols, while looking for the start
      while not Ada.Text_IO.End_Of_File (File => File_Handle) loop
         declare
            Line : constant String := Ada.Text_IO.Get_Line (File_Handle);
         begin

            for Index in Line'Range loop
               declare
                  M_Row : constant Integer := Row + 1;
                  M_Col : constant Integer := Index;
               begin
                  Map (M_Row, M_Col) := Line (Index);
               end;
            end loop;
            Row := Row + 1;
         end;
      end loop;
      Ada.Text_IO.Close (File_Handle);

      return Map;
   end Read_Map;

   function Double_Size (Original : Maze_Symbols) return Maze_Symbols
   is
      D_Height : Integer;
      D_Width : Integer;
   begin
      for Index in Original'Range (1) loop
         D_Height := Index;
      end loop;

      for Index in Original'Range (2) loop
         D_Width := Index;
      end loop;

      Ada.Text_IO.Put_Line ("Last indices " & D_Height'Image
         & "x" & D_Width'Image);

      declare
         Doubled : Maze_Symbols (0 .. 2 * D_Height, 0 .. 2 * D_Width);
         M_Row : Integer;
         M_Col : Integer;
      begin

         --  Fill the borders with ground
         for Index in Doubled'Range (2) loop
            Doubled (0, Index) := '.';
            Doubled (2 * D_Height, Index) := '.';
         end loop;

         for Index in Doubled'Range (1) loop
            Doubled (Index, 0) := '.';
            Doubled (Index, 2 * D_Width) := '.';
         end loop;

         Ada.Text_IO.Put_Line ("Maze size " & D_Height'Image & D_Width'Image);

         --  Start by placing all the known symbols
         for Row in Original'Range (1) loop
            for Col in Original'Range (2) loop

               M_Row := 2 * (Row - 1) + 1;
               M_Col := 2 * (Col - 1) + 1;

               Doubled (M_Row, M_Col) := Original (Row, Col);
               if Col < D_Width then
                  if Contains (Connect_West, Original (Row, Col))
                     and then Contains (Connect_East, Original (Row, Col + 1))
                  then
                     Doubled (M_Row, M_Col + 1) := '-';
                  else
                     Doubled (M_Row, M_Col + 1) := '#';
                  end if;

                  if Row < D_Height then
                     Doubled (M_Row + 1, M_Col + 1) := '#';
                  end if;
               end if;

               if Row < D_Height then
                  if Contains (Connect_North, Original (Row, Col))
                     and then Contains (Connect_South, Original (Row + 1, Col))
                  then
                     Doubled (M_Row + 1, M_Col) := '|';
                  else
                     Doubled (M_Row + 1, M_Col) := '#';
                  end if;
               end if;
            end loop;
         end loop;
         return Doubled;
      end;
   end Double_Size;

   --  Initializes the path markers
   procedure Initialise_Path (Path : in out Maze_Path)
   is
   begin
      --  Initialize the maze path
      for Row in Path'Range (1) loop
         for Col in Path'Range (2) loop
            Path (Row, Col) := -1;
         end loop;
      end loop;
   end Initialise_Path;

   --  Finds the starting position in a map
   function Find_Start (Maze : Maze_Symbols) return Point
   is
   begin
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            if Maze (Row, Col) = 'S' then
               return (Row, Col);
            end if;
         end loop;
      end loop;
      return (-1, -1);
   end Find_Start;

   function Mark_Path (
      Queue : in out Visit_Queue.List;
      Map : Maze_Symbols;
      Path : in out Maze_Path
   ) return Point
   is
      Next : Point;
   begin
      while not Visit_Queue.Is_Empty (Queue) loop
         Next := Visit_Queue.First_Element (Queue);
         Visit_Queue.Delete_First (Queue);

         declare
            Symbol : constant Character :=
               Map (Next.Row, Next.Column);
            Row : constant Integer := Next.Row;
            Column : constant Integer := Next.Column;
            North : constant Integer := Row - 1;
            South : constant Integer := Row + 1;
            East : constant Integer := Column + 1;
            West : constant Integer := Column - 1;
         begin

            if (Path (Row, Column) = -1)
               and then (Symbol /= '.')
            then
               Path (Row, Column) := 0;
               if Symbol = '|' then
                  if Path (South, Column) = -1
                     or else Path (North, Column) = -1
                  then
                     Visit_Queue.Append (Queue, (North, Column));
                     Visit_Queue.Append (Queue, (South, Column));
                  else
                     Ada.Text_IO.Put_Line ("| join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = '-' then
                  if Path (Row, East) = -1
                     or else Path (Row, West) = -1
                  then

                     Visit_Queue.Append (Queue, (Row, East));
                     Visit_Queue.Append (Queue, (Row, West));
                  else
                     Ada.Text_IO.Put_Line ("- join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = 'L' then
                  if Path (North, Column) = -1
                     or else Path (Row, East) = -1
                  then

                     Visit_Queue.Append (Queue, (North, Column));
                     Visit_Queue.Append (Queue, (Row, East));
                  else
                     Ada.Text_IO.Put_Line ("L join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = 'J' then
                  if Path (Row, West) = -1
                     or else Path (North, Column) = -1
                  then

                     Visit_Queue.Append (Queue, (North, Column));
                     Visit_Queue.Append (Queue, (Row, West));
                  else
                     Ada.Text_IO.Put_Line ("J join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = '7' then
                  if Path (Row, West) = -1
                     or else Path (South, Column) = -1
                  then

                     Visit_Queue.Append (Queue, (South, Column));
                     Visit_Queue.Append (Queue, (Row, West));
                  else
                     Ada.Text_IO.Put_Line ("7 join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = 'F' then
                  if Path (Row, East) = -1
                     or else Path (South, Column) = -1
                  then

                     Visit_Queue.Append (Queue, (Row, East));
                     Visit_Queue.Append (Queue, (South, Column));
                  else
                     Ada.Text_IO.Put_Line ("F join at: " &
                        Row'Image & Column'Image);
                     return (Row, Column);
                  end if;
               elsif Symbol = 'S' then
                  -- Nothing to add?
                  Ada.Text_IO.Put_Line ("S at: " & Row'Image & Column'Image);
               end if;
            end if;
         end;
      end loop;
      return (-1, -1);
   end Mark_Path;

--  Given the dimension of the map, starts reading in the map.
   procedure Find_Path (Maze : in out Maze_Symbols)
   is

      Path : Maze_Path (Maze'Range (1), Maze'Range (2));
      Row : Integer := 0;
      Start : constant Point := Find_Start (Maze);
      S_Row : constant Integer := Start.Row;
      S_Col : constant Integer := Start.Column;
      Queue : Visit_Queue.List;
      Join : Point;
   begin
      Initialise_Path (Path);

      --  Mark the start
      Path (S_Row, S_Col) := 0;

      --  Add the initial steps
      if Contains (Connect_North, Maze (S_Row - 1, S_Col)) then
         Visit_Queue.Append (Queue, (S_Row - 1, S_Col));
      end if;
      if Contains (Connect_South, Maze (S_Row + 1, S_Col)) then
         Visit_Queue.Append (Queue, (S_Row + 1, S_Col));
      end if;
      if Contains (Connect_West, Maze (S_Row, S_Col - 1)) then
         Visit_Queue.Append (Queue, (S_Row, S_Col - 1));
      end if;
      if Contains (Connect_East, Maze (S_Row, S_Col + 1)) then
         Visit_Queue.Append (Queue, (S_Row, S_Col + 1));
      end if;

      --  Now we have the data, find the "join" mid point
      Join := Mark_Path (Queue, Maze, Path);

      --  Now, traverse the path one more time to mark the main path
      Initialise_Path (Path);
      Visit_Queue.Clear (Queue);
      Visit_Queue.Append (Queue, Join);
      Join := Mark_Path (Queue, Maze, Path);

      Ada.Text_IO.Put_Line ("Path markings");
      --  Diagnostic output for the path
      for Row in Path'Range (1) loop
         for Col in Path'Range (2) loop
            if Path (Row, Col) = -1 then
               Ada.Text_IO.Put(".");
            else
               Ada.Text_IO.Put ("#");
            end if;
         end loop;
         Ada.Text_IO.Put_Line ("");
      end loop;

      --  Go through the path and remove any path segments not part of the
      --  main path
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            if Contains (Path_Symbols, Maze (Row, Col))
               and then Path (Row, Col) = -1 
            then
               if Row rem 2 = 1
                  and then Col rem 2 = 1
               then
                  Maze (Row, Col) := '.';
               else
                  Maze (Row, Col) := '#';
               end if;
            end if;
         end loop;
      end loop;
   end Find_Path;

begin

   if Ada.Command_Line.Argument_Count < 1
   then
      Ada.Text_IO.Put_Line ("Missing input file");
      return;
   end if;

   Ada.Text_IO.Put_Line ("Reading file: " & Ada.Command_Line.Argument (1));
   declare
      Original_Maze : Maze_Symbols := Read_Map (Ada.Command_Line.Argument (1));
      Maze : Maze_Symbols := Double_Size (Original_Maze);
      Count : Integer := 0;

      Queue : Visit_Queue.List;
   begin

      Ada.Text_IO.Put_Line ("Original maze");
      Print_Maze (Original_Maze);

      Ada.Text_IO.Put_Line ("Doubled maze");
      Print_Maze (Maze);

      Find_Path (Maze);
      Ada.Text_IO.Put_Line ("After find path");
      Print_Maze (Maze);

      --  Flood fill from the top left corner
      Visit_Queue.Append (Queue, (0, 0));
      while not Visit_Queue.Is_Empty (Queue) loop
         declare
            Next : Point := Visit_Queue.First_Element (Queue);
         begin
            Visit_Queue.Delete_First (Queue);
            if Next.Row in Maze'Range (1)
               and then Next.Column in Maze'Range (2)
            then
               if Maze (Next.Row, Next.Column) = '.' or else
                  Maze (Next.Row, Next.Column) = '#'
               then
                  Maze (Next.Row, Next.Column) := ' ';
                  Visit_Queue.Append (Queue, (Next.Row - 1, Next.Column));
                  Visit_Queue.Append (Queue, (Next.Row + 1, Next.Column));
                  Visit_Queue.Append (Queue, (Next.Row, Next.Column - 1));
                  Visit_Queue.Append (Queue, (Next.Row, Next.Column + 1));
               end if;
            end if;
         end;
      end loop;

      --  Count the remaining tiles
      Ada.Text_IO.Put_Line ("After walk");
      Print_Maze (Maze);
      for Row in Maze'Range (1) loop
         for Col in Maze'Range (2) loop
            if (Maze (Row, Col) = '.') then
               Count := Count + 1;
            end if;
         end loop;
      end loop;
      Ada.Text_IO.Put_Line ("Enclosed count:" & Count'Image);

   end;
end Maze;
