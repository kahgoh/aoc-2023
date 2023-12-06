(module
  (memory (export "mem") 1)

  (global $FALSE i32 (i32.const 0))
  (global $LF i32 (i32.const 10))
  (global $EMPTY i32 (i32.const 46))
  (global $0 i32 (i32.const 48))
  (global $9 i32 (i32.const 57))

  (func $measure (param $map i32) (result i32)
    (local $index i32)
    (local $next i32)

    (loop $count_length
      (local.set 
        $next
        (i32.load8_u 
          (i32.add 
            (local.get $map)
            (local.get $index)
          )
        )
      )

      (if 
        (i32.ne
          (local.get $next)
          (global.get $LF)
        )

        (then 
          (local.set 
            $index 
            (i32.add 
              (local.get $index)
              (i32.const 1)
            )
          )

          (br $count_length)
        )
      )
    )

    (return (local.get $index))
  )

  (func $toIndex (param $map i32) (param $mapDim i32) (param $row i32) (param $col i32) (result i32)
    (return 
      (i32.add
        (local.get $map)
        (i32.add                      ;; Convert to byte index
          (i32.mul
            (local.get $row)
            (i32.add                  ;; Add one to columns to account for line-feed character
              (local.get $mapDim)
              (i32.const 1)
            )
          )
          (local.get $col)
        )
      )
    )
  )

  (func $getValue (param $map i32) (param $mapDim i32) (param $row i32) (param $col i32) (result i32)
    (if (i32.eq (local.get $row) (i32.const -1))
      (then (return (global.get $EMPTY)))
    )
    (if (i32.eq (local.get $col) (i32.const -1))
      (then (return (global.get $EMPTY)))
    )
    (if (i32.eq (local.get $row) (local.get $mapDim))
      (then (return (global.get $EMPTY)))
    )
    (if (i32.eq (local.get $col) (local.get $mapDim))
      (then (return (global.get $EMPTY)))
    )
    
    (return 
      (i32.load8_u 
        (call $toIndex
          (local.get $map)
          (local.get $mapDim)
          (local.get $row)
          (local.get $col)
        )
      )
    )
  )

  (func $emptyCell (param $map i32) (param $mapDim i32) (param $row i32) (param $col i32)
        (if (i32.eq (local.get $row) (i32.const -1))
      (then (return))
    )
    (if (i32.eq (local.get $col) (i32.const -1))
      (then (return))
    )
    (if (i32.eq (local.get $row) (local.get $mapDim))
      (then (return))
    )
    (if (i32.eq (local.get $col) (local.get $mapDim))
      (then (return))
    )

    (i32.store8
      (call $toIndex
        (local.get $map)
        (local.get $mapDim)
        (local.get $row)
        (local.get $col)
      )
      (global.get $EMPTY)
    )
  )

  (func $isDigit (param $byte i32) (result i32)
    (return
      (i32.and
        (i32.ge_u (local.get $byte) (global.get $0))
        (i32.le_u (local.get $byte) (global.get $9))
      )
    )
  )

  (func $isSymbol (param $byte i32) (result i32)
    (return
      (i32.and
        ;; Can't be a digit
        (i32.eq 
          (call $isDigit (local.get $byte))
          (global.get $FALSE)
        )
      
        ;; Can't be empty space
        (i32.ne
          (local.get $byte)
          (global.get $EMPTY)
        )
      )
    )
  )

  (func $readNum (param $map i32) (param $dim i32) (param $row i32) (param $col i32) (result i32)
    (local $num i32)
    (local $cursor i32)
    (local $symbol i32)

    (local.set $cursor (local.get $col))

    ;; If we are not starting at a digit, straight away return 0
    (if
      (i32.eq
        (global.get $FALSE)
        (call $isDigit 
          (call $getValue
            (local.get $map)
            (local.get $dim)
            (local.get $row)
            (local.get $cursor)
          )
        )
      )
      (then
        (return (i32.const 0))
      )
    )


    ;; First, find the beginning of the number.
    (loop $findStartLoop
      (local.set $symbol
        (call $getValue
          (local.get $map)
          (local.get $dim)
          (local.get $row)
          (local.get $cursor)
        )
      )

      (if 
        (call $isDigit (local.get $symbol))
        (then
          (local.set $cursor
            (i32.sub
              (local.get $cursor)
              (i32.const 1)
            )
          )
          (br $findStartLoop)
        )
      )
    )

    ;; Now read the number left to right
    ;; Remember, cursor is now at a non-digit, so have to start at next byte
    (loop $readDigitsLoop

      ;; Move cursor forward one
      (local.set $cursor
        (i32.add
          (local.get $cursor)
          (i32.const 1)
        )
      )

      ;; What's the symbol?
      (local.set $symbol
        (call $getValue
          (local.get $map)
          (local.get $dim)
          (local.get $row)
          (local.get $cursor)
        )
      )

      (if 
        (call $isDigit (local.get $symbol))
        (then 
          (local.set $num
            ;; Append the digit
            (i32.add
              (i32.mul
                (local.get $num)
                (i32.const 10)
              )
              (i32.sub
                (local.get $symbol)
                (global.get $0)
              )
            )
          )
          ( ;; Mark the cell as visited
            call $emptyCell
              (local.get $map)
              (local.get $dim)
              (local.get $row)
              (local.get $cursor)
          )
          ( ;; Keep reading until no more digits
            br $readDigitsLoop
          )
        )
      )
    )
    (return (local.get $num))
  )

  (func $sumNums (param $map i32) (param $dim i32) (param $row i32) (param $col i32) (result i32)
    (local $res i32)
    (local $rowUp i32)
    (local $rowDown i32)
    (local $colLeft i32)
    (local $colRight i32)

    (local.set $rowUp (i32.sub (local.get $row) (i32.const 1)))
    (local.set $rowDown (i32.add (local.get $row) (i32.const 1)))
    (local.set $colLeft (i32.sub (local.get $col) (i32.const 1)))
    (local.set $colRight (i32.add (local.get $col) (i32.const 1)))

    (call $readNum (local.get $map) (local.get $dim) (local.get $rowUp) (local.get $colLeft))
    (call $readNum (local.get $map) (local.get $dim) (local.get $rowUp) (local.get $col))
    (call $readNum (local.get $map) (local.get $dim) (local.get $rowUp) (local.get $colRight))
    (call $readNum (local.get $map) (local.get $dim) (local.get $row) (local.get $colLeft))
    (call $readNum (local.get $map) (local.get $dim) (local.get $row) (local.get $colRight))
    (call $readNum (local.get $map) (local.get $dim) (local.get $rowDown) (local.get $colLeft))
    (call $readNum (local.get $map) (local.get $dim) (local.get $rowDown) (local.get $col))
    (call $readNum (local.get $map) (local.get $dim) (local.get $rowDown) (local.get $colRight))

    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    i32.add
    return
  )

  (func (export "solve") (param $map i32) (result i32)
    (local $dimension i32)
    (local $row i32)
    (local $col i32)
    (local $current i32)
    (local $sum i32)
    (local.set $dimension (call $measure (local.get $map)))

    (loop $row_loop
      (local.set $col (i32.const 0))

      (loop $col_loop
        (local.set $col
          (i32.add
            (local.get $col)
            (i32.const 1)
          )
        )

        (local.set $current
          (call $getValue 
            (local.get $map)
            (local.get $dimension)
            (local.get $row)
            (local.get $col) 
          )
        )

        (if
          (call $isSymbol
            (local.get $current)
          )

          (then
            (local.set $sum
              (i32.add
                (local.get $sum)
                (call $sumNums
                  (local.get $map)
                  (local.get $dimension)
                  (local.get $row)
                  (local.get $col)
                )
              )
            )
          )
        )

        (br_if $col_loop 
          (i32.lt_u
            (local.get $col)
            (local.get $dimension)
          )
        )
      )

      (local.set $row
        (i32.add
          (local.get $row)
          (i32.const 1)
        )
      )

      (br_if $row_loop 
        (i32.lt_u
          (local.get $row)
          (local.get $dimension)
        )
      )
    )

    (return 
      (local.get $sum)
    )
  )
)
