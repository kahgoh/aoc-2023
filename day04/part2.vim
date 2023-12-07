function Score(winning, chosen)
  " Parse the winning numbers
  let winSet = {}
  for num in split(a:winning)
    let winSet[str2nr(num)]=1
  endfor

  " Count the number of winning numbers
  let matches=0
  for num in split(a:chosen)
    if has_key(winSet, str2nr(num))
      let matches=matches+1
    endif
  endfor

  return matches
endfunction

function! Solve() abort
  let l=1
  let result=0
  let copies={}
  wh l <= line("$")
    let content=split(getline(l), ":")[1]
    let game=split(content, "|")
    let score=Score(game[0], game[1])
    
    let multiplier=get(copies, l, 1)
    let result=result + multiplier
    if score > 0
      for offset in range(1, score)
        let key=offset + l
        let copies[key] = multiplier + get(copies, key, 1)
      endfor
    end
    let l = l + 1  
  endwh
  echo result
endfunction
