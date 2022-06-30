" Vim syntax file
" Language: Bruijn

if exists("b:current_syntax")
  finish
endif

let s:bruijn_syntax_keywords = {
      \ 'bruijnInstruction' :[":test "
      \ ,                     ":import "
      \ ,                     ":print "
      \ ,                    ]
      \ , }

function! s:syntax_keyword(dict)
  for key in keys(a:dict)
    execute 'syntax keyword' key join(a:dict[key], ' ')
  endfor
endfunction

call s:syntax_keyword(s:bruijn_syntax_keywords)

syntax match bruijnApplication /[()]/
syntax match bruijnAbstraction /[[\]]/
syntax match bruijnIndex display "\d"
syntax match bruijnDefinition /^\S\+/

syntax region bruijnCommentLine start="# " end="$"

" this might be weird but because of bruijn's limited
" functionality it's okay to use the wrong constants
" for better looks
highlight default link bruijnIndex Special
highlight default link bruijnDefinition Function
highlight default link bruijnInstruction Keyword
highlight default link bruijnAbstraction Boolean
highlight default link bruijnApplication String
highlight default link bruijnCommentLine Comment

delfunction s:syntax_keyword

let b:current_syntax = "bruijn"
