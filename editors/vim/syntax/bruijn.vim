" Vim syntax file
" Language: Bruijn

if exists("b:current_syntax")
  finish
endif

syn match bruijnApplication /[()]/
syn match bruijnAbstraction /[[\]]/
syn match bruijnIndex /\([^0-9A-Za-z]\)\@<=\d\([^0-9]\)\@=/
syn match bruijnChar /'\@<=.'\@=/
syn match bruijnNumber /([+-]\d\+[dubt]\?)/
syn match bruijnFloat /([+-]\d\+\.\d\+[qr]\?)/
syn match bruijnComplex /([+-]\d\+\.\d\+[+-]\d\+\.\d\+i)/
syn match bruijnDefinition /^\t*\S\+/
syn match bruijnType /\( ⧗ \)\@<=.*$/
syn match bruijnTypeDelim / ⧗ /
syn match bruijnKeyword /:test\|:import\|:input/
syn match bruijnNamespace /[A-Z][a-z]*\(\.\)\@=/
syn match bruijnNamespaceDelim /\([A-Z][a-z]*\)\@<=\./

syn region bruijnCommentLine start="^# " end="$" oneline
syn region bruijnString start=+"+ end=+"+ oneline

hi def link bruijnIndex Special
hi def link bruijnNumber Number
hi def link bruijnFloat Number
hi def link bruijnComplex Number
hi def link bruijnString String
hi def link bruijnChar String
hi def link bruijnDefinition Define
hi def link bruijnType Structure
hi def link bruijnTypeDelim Define
hi def link bruijnKeyword Macro
hi def link bruijnNamespace Type
hi def link bruijnNamespaceDelim Special
hi def link bruijnAbstraction Function
hi def link bruijnApplication Statement
hi def link bruijnCommentLine Comment

" complete using space or C-]
abbreviate <buffer> :: ⧗
abbreviate <buffer> > ‣
abbreviate <buffer> <> ∅
abbreviate <buffer> ? …
abbreviate <buffer> && ⋀
abbreviate <buffer> \|\| ⋁
abbreviate <buffer> sum ∑
abbreviate <buffer> prod ∏
abbreviate <buffer> rprod ∐
abbreviate <buffer> infty ∞
abbreviate <buffer> . ∘
abbreviate <buffer> * ⋅
abbreviate <buffer> ** ⋆
abbreviate <buffer> => ⇒
abbreviate <buffer> <=> ⇔
abbreviate <buffer> -> →
abbreviate <buffer> <-> ↔
abbreviate <buffer> -^ ↑
abbreviate <buffer> bra ⟨
abbreviate <buffer> ket ⟩
abbreviate <buffer> bbra ⟪
abbreviate <buffer> kket ⟫
abbreviate <buffer> <=? ≤
abbreviate <buffer> >=? ≥
abbreviate <buffer> /= ≠
abbreviate <buffer> ~= ≈
abbreviate <buffer> ! ¬
abbreviate <buffer> _0 ₀
abbreviate <buffer> _1 ₁
abbreviate <buffer> _2 ₂
abbreviate <buffer> _3 ₃
abbreviate <buffer> _4 ₄
abbreviate <buffer> _5 ₅
abbreviate <buffer> _6 ₆
abbreviate <buffer> _7 ₇
abbreviate <buffer> _8 ₈
abbreviate <buffer> _9 ₉
abbreviate <buffer> _+ ₊
abbreviate <buffer> _- ₋
abbreviate <buffer> _= ₌
abbreviate <buffer> _( ₍
abbreviate <buffer> _) ₎
abbreviate <buffer> ^0 ⁰
abbreviate <buffer> ^1 ¹
abbreviate <buffer> ^2 ²
abbreviate <buffer> ^3 ³
abbreviate <buffer> ^4 ⁴
abbreviate <buffer> ^5 ⁵
abbreviate <buffer> ^6 ⁶
abbreviate <buffer> ^7 ⁷
abbreviate <buffer> ^8 ⁸
abbreviate <buffer> ^9 ⁹
abbreviate <buffer> ^+ ⁺
abbreviate <buffer> ^- ⁻
abbreviate <buffer> ^= ⁼
abbreviate <buffer> ^( ⁽
abbreviate <buffer> ^) ⁾

" === Greek letters === 
abbreviate <buffer> Alpha Α
abbreviate <buffer> alpha α
abbreviate <buffer> Beta Β
abbreviate <buffer> beta β
abbreviate <buffer> Gamma Γ
abbreviate <buffer> gamma γ
abbreviate <buffer> Delta Δ
abbreviate <buffer> delta δ
abbreviate <buffer> Epsilon Ε
abbreviate <buffer> epsilon ε
abbreviate <buffer> varepsilon ϵ
abbreviate <buffer> Zeta Ζ
abbreviate <buffer> zeta ζ
abbreviate <buffer> Eta Η
abbreviate <buffer> eta η
abbreviate <buffer> Theta Θ
abbreviate <buffer> theta θ
abbreviate <buffer> Iota Ι
abbreviate <buffer> iota ι
abbreviate <buffer> Kappa Κ
abbreviate <buffer> kappa κ
abbreviate <buffer> Lambda Λ
abbreviate <buffer> lambda λ
abbreviate <buffer> Mu Μ
abbreviate <buffer> mu μ
abbreviate <buffer> Nu Ν
abbreviate <buffer> nu ν
abbreviate <buffer> Xi Ξ
abbreviate <buffer> xi ξ
abbreviate <buffer> Omicron Ο
abbreviate <buffer> omicron ο
abbreviate <buffer> Pi Π
abbreviate <buffer> pi π
abbreviate <buffer> Rho Ρ
abbreviate <buffer> rho ρ
abbreviate <buffer> Sigma Σ
abbreviate <buffer> sigma σ
abbreviate <buffer> Tau Τ
abbreviate <buffer> tau τ
abbreviate <buffer> Upsilon Υ
abbreviate <buffer> upsilon υ
abbreviate <buffer> Phi Φ
abbreviate <buffer> phi φ
abbreviate <buffer> varphi ϕ
abbreviate <buffer> Chi Χ
abbreviate <buffer> chi χ
abbreviate <buffer> Psi Ψ
abbreviate <buffer> psi ψ
abbreviate <buffer> Omega Ω
abbreviate <buffer> omega ω

" let b:current_syntax = "bruijn"
" let &cpo = s:cpo_save
" unlet! s:cpo_save
