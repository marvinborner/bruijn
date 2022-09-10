" Vim syntax file
" Language: Bruijn

if exists("b:current_syntax")
  finish
endif

syn match bruijnApplication /[()]/
syn match bruijnAbstraction /[[\]]/
syn match bruijnIndex /\([^0-9]\)\@<=\d\([^0-9]\)\@=/
syn match bruijnNumber /([+-]\d\+)/
syn match bruijnDefinition /^\t*\S\+/
syn match bruijnKeyword /:test\|:import\|:input/
syn match bruijnNamespace /[A-Z][a-z]*\(\.\)\@=/
syn match bruijnNamespaceDelim /\([A-Z][a-z]*\)\@<=\./

syn region bruijnCommentLine start="^# " end="$" oneline
syn region bruijnString start=+"+ end=+"+ oneline
syn region bruijnChar start=+'+ end=+'+ oneline

hi def link bruijnIndex Special
hi def link bruijnNumber Number
hi def link bruijnString String
hi def link bruijnChar String
hi def link bruijnDefinition Define
hi def link bruijnKeyword Macro
hi def link bruijnNamespace Type
hi def link bruijnNamespaceDelim Special
hi def link bruijnAbstraction Function
hi def link bruijnApplication Statement
hi def link bruijnCommentLine Comment

let b:current_syntax = "bruijn"

" complete using space or C-]
abbreviate > ‣
abbreviate ? …
abbreviate && ⋀
abbreviate \|\| ⋁
abbreviate sum ∑
abbreviate prod ∏
abbreviate rprod ∐
abbreviate infty ∞
abbreviate . ∘
abbreviate * ⋅
abbreviate ** ⋆
abbreviate => ⇒
abbreviate <=> ⇔
abbreviate -> →
abbreviate <-> ↔
abbreviate -^ ↑
abbreviate :: ∷
abbreviate bra ⟨
abbreviate ket ⟩
abbreviate bbra ⟪
abbreviate kket ⟫
abbreviate <=? ≤
abbreviate >=? ≥
abbreviate ! ¬
abbreviate _0 ₀
abbreviate _1 ₁
abbreviate _2 ₂
abbreviate _3 ₃
abbreviate _4 ₄
abbreviate _5 ₅
abbreviate _6 ₆
abbreviate _7 ₇
abbreviate _8 ₈
abbreviate _9 ₉
abbreviate _+ ₊
abbreviate _- ₋
abbreviate _= ₌
abbreviate _( ₍
abbreviate _) ₎
abbreviate ^0 ⁰
abbreviate ^1 ¹
abbreviate ^2 ²
abbreviate ^3 ³
abbreviate ^4 ⁴
abbreviate ^5 ⁵
abbreviate ^6 ⁶
abbreviate ^7 ⁷
abbreviate ^8 ⁸
abbreviate ^9 ⁹
abbreviate ^\+ ⁺
abbreviate ^- ⁻
abbreviate ^= ⁼
abbreviate ^\( ⁽
abbreviate ^\) ⁾

" === Greek letters === 
abbreviate Alpha Α
abbreviate alpha α
abbreviate Beta Β
abbreviate beta β
abbreviate Gamma Γ
abbreviate gamma γ
abbreviate Delta Δ
abbreviate delta δ
abbreviate Epsilon Ε
abbreviate epsilon ε
abbreviate varepsilon ϵ
abbreviate Zeta Ζ
abbreviate zeta ζ
abbreviate Eta Η
abbreviate eta η
abbreviate Theta Θ
abbreviate theta θ
abbreviate Iota Ι
abbreviate iota ι
abbreviate Kappa Κ
abbreviate kappa κ
abbreviate Lambda Λ
abbreviate lambda λ
abbreviate Mu Μ
abbreviate mu μ
abbreviate Nu Ν
abbreviate nu ν
abbreviate Xi Ξ
abbreviate xi ξ
abbreviate Omicron Ο
abbreviate omicron ο
abbreviate Pi Π
abbreviate pi π
abbreviate Rho Ρ
abbreviate rho ρ
abbreviate Sigma Σ
abbreviate sigma σ
abbreviate Tau Τ
abbreviate tau τ
abbreviate Upsilon Υ
abbreviate upsilon υ
abbreviate Phi Φ
abbreviate phi φ
abbreviate varphi ϕ
abbreviate Chi Χ
abbreviate chi χ
abbreviate Psi Ψ
abbreviate psi ψ
abbreviate Omega Ω
abbreviate omega ω
