with Ada.Command_Line;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Text_IO;

procedure Maze is
   type Dimension is record
      Width : Integer := 0;
      Height : Integer := 0;
   end record;

   type Sym_Arr is array (Integer range <>) of Character;

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
         Ada.Text_IO.Put_Line ("Got line: " & Line);
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

   --  Check if an array contains a specific element
   function Contains (Container : Sym_Arr; Elem : Character) return Boolean
   is
   begin
      for Index in Container'Range loop
         if Container (Index) = Elem then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

   --  Given the dimension of the map, starts reading in the map.
   function Load_Map (File_Name : String; Dim : Dimension) return Integer
   is
      type Work_Visit is record
         Row : Integer;
         Column : Integer;
         Distance : Integer;
      end record;

      package Work_Queue is
         new Ada.Containers.Doubly_Linked_Lists (Element_Type => Work_Visit);

      File_Handle : Ada.Text_IO.File_Type;
      Distances : array (0 .. Dim.Height + 1, 0 .. Dim.Width + 1) of Integer;
      Map : array (0 .. Dim.Height + 1, 0 .. Dim.Width + 1) of Character;
      Row : Integer := 0;
      S_Row : Integer;
      S_Col : Integer;
      Queue : Work_Queue.List;
   begin
      Ada.Text_IO.Open (File => File_Handle,
         Mode => Ada.Text_IO.In_File,
         Name => File_Name);

      --  Fill the borders with ground
      for Index in 0 .. Dim.Width + 1 loop
         Map (0, Index) := '.';
         Map (Dim.Height + 1, Index) := '.';
      end loop;

      for Index in 0 .. Dim.Height + 1 loop
         Map (Index, 0) := '.';
         Map (Index, Dim.Width + 1) := '.';
      end loop;

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

                  if Line (Index) = 'S' then
                     Ada.Text_IO.Put_Line ("Found S at index " &
                        index'Image & ", row " & M_Row'Image);
                     Ada.Text_IO.Put_Line ("Start at " &
                        M_Row'Image & M_Col'Image);

                     S_Row := M_Row;
                     S_Col := M_Col;
                     Distances (M_Row, M_Col) := 0;
                  else
                     Distances (M_Row, M_Col) := -1;
                  end if;
               end;
            end loop;
            Row := Row + 1;
         end;
      end loop;

      --  Add the initial steps
      if Contains (('|', '7', 'F'), Map (S_Row - 1, S_Col)) then
         Work_Queue.Append (Queue,
               (Row => S_Row - 1, Column => S_Col, Distance => 1));
      end if;
      if Contains (('|', 'J', 'L'), Map (S_Row + 1, S_Col)) then
         Work_Queue.Append (Queue,
            (Row => S_Row + 1, Column => S_Col, Distance => 1));
      end if;
      if Contains (('-', 'L', 'F'), Map (S_Row, S_Col - 1)) then
         Work_Queue.Append (Queue,
            (Row => S_Row, Column => S_Col - 1, Distance => 1));
      end if;
      if Contains (('-', 'J', '7'), Map (S_Row, S_Col + 1)) then
         Work_Queue.Append (Queue,
            (Row => S_Row, Column => S_Col + 1, Distance => 1));
      end if;

      Ada.Text_IO.Close (File_Handle);

      --  Now we have the data, can start solving
      while not Work_Queue.Is_Empty (Queue) loop
         declare
            Next : constant Work_Visit := Work_Queue.First_Element (Queue);
            Symbol : constant Character := Map (Next.Row, Next.Column);
            Row : constant Integer := Next.Row;
            Column : constant Integer := Next.Column;
            North : constant Integer := Row - 1;
            South : constant Integer := Row + 1;
            East : constant Integer := Column + 1;
            West : constant Integer := Column - 1;
            Next_Dist : constant Integer := Next.Distance + 1;
         begin

            Work_Queue.Delete_First (Queue);

            if (Distances (Next.Row, Next.Column) = -1)
               and then (Symbol /= '.')
            then
               Ada.Text_IO.Put_Line ("Processing node: " &
                  Next.Row'Image &
                  Next.Column'Image &
                  Next.Distance'Image &
                  " " & Symbol);

               Distances (Next.Row, Next.Column) := Next.Distance;
               if Symbol = '|' then
                  if Distances (South, Column) = -1
                     or else Distances (North, Column) = -1
                  then

                     Work_Queue.Append (Queue,
                        (North, Column, Next_Dist));
                     Work_Queue.Append (Queue,
                        (South, Column, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               elsif Symbol = '-' then
                  if Distances (Row, East) = -1
                     or else Distances (Row, West) = -1
                  then

                     Work_Queue.Append (Queue,
                        (Row, East, Next_Dist));
                     Work_Queue.Append (Queue,
                        (Row, West, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               elsif Symbol = 'L' then
                  if Distances (North, Column) = -1
                     or else Distances (Row, East) = -1
                  then

                     Work_Queue.Append (Queue,
                        (North, Column, Next_Dist));
                     Work_Queue.Append (Queue,
                        (Row, East, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               elsif Symbol = 'J' then
                  if Distances (Row, West) = -1
                     or else Distances (North, Column) = -1
                  then

                     Work_Queue.Append (Queue,
                        (North, Column, Next_Dist));
                     Work_Queue.Append (Queue,
                        (Row, West, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               elsif Symbol = '7' then
                  if Distances (Row, West) = -1
                     or else Distances (South, Column) = -1
                  then

                     Work_Queue.Append (Queue,
                        (South, Column, Next_Dist));
                     Work_Queue.Append (Queue,
                        (Row, West, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               elsif Symbol = 'F' then
                  if Distances (Row, East) = -1
                     or else Distances (South, Column) = -1
                  then

                     Work_Queue.Append (Queue,
                        (Row, East, Next_Dist));
                     Work_Queue.Append (Queue,
                        (South, Column, Next_Dist));
                  else
                     Ada.Text_IO.Put_Line ("Join at: " &
                        Row'Image & Column'Image);
                     return Next.Distance;
                  end if;
               end if;
            end if;
         end;
      end loop;
      return 0;
   end Load_Map;

begin

   if Ada.Command_Line.Argument_Count < 1
   then
      Ada.Text_IO.Put_Line ("Missing input file");
      return;
   end if;

   Ada.Text_IO.Put_Line ("Reading file: " & Ada.Command_Line.Argument (1));
   declare
      Dim : constant Dimension := Get_Dimension
         (Ada.Command_Line.Argument (1));
      Result : Integer;
   begin
      Result := Load_Map (Ada.Command_Line.Argument (1), Dim);
      Ada.Text_IO.Put_Line ("Result: " & Result'Image);
   end;
end Maze;
