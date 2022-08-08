" Vim syntax file
" Language: Bruijn

if exists("b:current_syntax")
  finish
endif

syn match bruijnApplication /[()]/
syn match bruijnAbstraction /[[\]]/
syn match bruijnIndex /\([^0-9]\)\@<=\d\([^0-9]\)\@=/
syn match bruijnNumber /[+-]\d\+/
syn match bruijnDefinition /^\t*\S\+/
syn match bruijnKeyword /:test\|:import\|:print/
syn match bruijnNamespace /[A-Z][a-z]*\(\.\)\@=/
syn match bruijnNamespaceDelim /\([A-Z][a-z]*\)\@<=\./

syn region bruijnCommentLine start="^# " end="$"

hi def link bruijnIndex Special
hi def link bruijnNumber Number
hi def link bruijnDefinition Define
hi def link bruijnKeyword Macro
hi def link bruijnNamespace Type
hi def link bruijnNamespaceDelim Special
hi def link bruijnAbstraction Function
hi def link bruijnApplication String
hi def link bruijnCommentLine Comment

let b:current_syntax = "bruijn"
