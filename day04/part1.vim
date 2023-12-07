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

  let result=0
  if matches > 0
    let result=pow(2, matches-1)
  endif
  return result
endfunction

function! Solve() abort
  let l=1
  let result=0
  wh l <= line("$")
    let content=split(getline(l), ":")[1]
    let game=split(content, "|")
    let result=result + Score(game[0], game[1])
    let l = l + 1  
  endwh
  echo result
endfunction
