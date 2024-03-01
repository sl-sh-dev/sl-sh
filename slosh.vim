" Vim syntax file
" Language:    SL-SH Lisp
" Maintainer:  Steven Stanfield <stanfield@scarecrowtech.com>
" Last Change: Nov 10, 2021
" Version:     1
" URL:	https://github.com/sstanfield/slvm
"
"  Derived from lisp.vim version 31 by Charles E. Campbell <NcampObell@SdrPchip.AorgM-NOSPAM>
"  Thanks to F Xavier Noria for a list of 978 Common Lisp symbols taken from HyperSpec
"  Clisp additions courtesy of http://clisp.cvs.sourceforge.net/*checkout*/clisp/clisp/emacs/lisp.vim

" ---------------------------------------------------------------------
"  Load Once: {{{1
if exists("b:current_syntax")
 finish
endif

if exists("g:slosh_isk")
 exe "setl isk=".g:slosh_isk
elseif (v:version == 704 && has("patch-7.4.1142")) || v:version > 704
 syn iskeyword 38,42,43,45,47-58,60-62,64-90,97-122,_
else
 setl isk=38,42,43,45,47-58,60-62,64-90,97-122,_
endif

" ---------------------------------------------------------------------
" Clusters: {{{1
syn cluster			sloshAtomCluster		contains=sloshAtomBarSymbol,sloshAtomList,sloshAtomNmbr0,sloshComment,sloshCommentRegion,sloshCommentDocStr,sloshFunc,sloshLeadWhite
syn cluster			sloshBaseListCluster	contains=sloshAtom,sloshAtomBarSymbol,sloshAtomMark,sloshBQList,sloshBarSymbol,sloshComment,sloshCommentRegion,sloshCommentDocStr,sloshConcat,sloshFunc,sloshKey,sloshList,sloshNumber,sloshEscapeSpecial,sloshSymbol,sloshVar,sloshLeadWhite
if exists("g:slosh_instring")
 syn cluster			sloshListCluster		contains=@sloshBaseListCluster,sloshString,sloshInString,sloshInStringString
else
 syn cluster			sloshListCluster		contains=@sloshBaseListCluster,sloshString
endif

"syn case ignore

" ---------------------------------------------------------------------
" Lists: {{{1
syn match sloshSymbol	contained	![^()'`,"; \t]\+!
syn match sloshBarSymbol	contained	!|..\{-}|!
syn region sloshParen0           matchgroup=hlLevel0 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen1
syn region sloshParen1 contained matchgroup=hlLevel1 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen2
syn region sloshParen2 contained matchgroup=hlLevel2 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen3
syn region sloshParen3 contained matchgroup=hlLevel3 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen4
syn region sloshParen4 contained matchgroup=hlLevel4 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen5
syn region sloshParen5 contained matchgroup=hlLevel5 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen6
syn region sloshParen6 contained matchgroup=hlLevel6 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen7
syn region sloshParen7 contained matchgroup=hlLevel7 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen8
syn region sloshParen8 contained matchgroup=hlLevel8 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen9
syn region sloshParen9 contained matchgroup=hlLevel9 start="`\=(" end=")" skip="|.\{-}|" contains=@sloshListCluster,sloshParen0

" ---------------------------------------------------------------------
" Atoms: {{{1
syn match sloshAtomMark		"'"
syn match sloshAtom		"'("me=e-1			contains=sloshAtomMark	nextgroup=sloshAtomList
syn match sloshAtom		"'[^ \t()]\+"			contains=sloshAtomMark
syn match sloshAtomBarSymbol	!'|..\{-}|!			contains=sloshAtomMark
syn region sloshAtom		start=+'"+			skip=+\\"+ end=+"+
syn region sloshAtomList		contained			matchgroup=Special start="("	skip="|.\{-}|" matchgroup=Special end=")"	contains=@sloshAtomCluster,sloshString,sloshEscapeSpecial
syn match sloshAtomNmbr		contained			"\<\d\+"
syn match sloshLeadWhite		contained			"^\s\+"

" ---------------------------------------------------------------------
" Standard slosh Functions and Macros: {{{1
syn keyword sloshFunc	def	do
syn keyword sloshFunc	defn	do
syn keyword sloshFunc	fn	macro	if
syn keyword sloshFunc	and	or	let	err
syn keyword sloshFunc	call/cc	defer	on-error	while
syn keyword sloshFunc	not	recur	this-fn	type
syn keyword sloshFunc	+	-	*	\
syn keyword sloshFunc	inc!	dec!	list	list-append
syn keyword sloshFunc	cons	car	cdr	xar!	xdr!
syn keyword sloshFunc	=	/=
" TODO- these dont work...	set!	eq?	equal?
syn keyword sloshFunc	<	>	<=	>=
syn keyword sloshFunc	pr	prn	load	eval

syn match   sloshFunc		"\<c[ad]\+r\>"

" ---------------------------------------------------------------------
" slosh Keywords (modifiers): {{{1
syn keyword sloshKey	#t	#f	nil

" ---------------------------------------------------------------------
" Standard slosh Variables: {{{1
" TODO XXX
syn keyword sloshVar		*applyhook*			*load-pathname*			*print-pprint-dispatch*

" ---------------------------------------------------------------------
" Strings: {{{1
syn region			sloshString			start=+"+ skip=+\\\\\|\\"+ end=+"+	contains=@Spell
if exists("g:slosh_instring")
 syn region			sloshInString			keepend matchgroup=Delimiter start=+"(+rs=s+1 skip=+|.\{-}|+ matchgroup=Delimiter end=+)"+ contains=@sloshBaseListCluster,sloshInStringString
 syn region			sloshInStringString		start=+\\"+ skip=+\\\\+ end=+\\"+ contained
endif

" ---------------------------------------------------------------------
" Numbers: supporting integers and floating point numbers {{{1
syn match sloshNumber		"-\=\(\.\d\+\|\d\+\(\.\d*\)\=\)\([dDeEfFlL][-+]\=\d\+\)\="
syn match sloshNumber		"-\=\(\d\+/\d\+\)"

syn match sloshEscapeSpecial		"\*\w[a-z_0-9-]*\*"
syn match sloshEscapeSpecial		!#|[^()'`,"; \t]\+|#!
syn match sloshEscapeSpecial		!#x\x\+!
syn match sloshEscapeSpecial		!#o\o\+!
syn match sloshEscapeSpecial		!#b[01]\+!
syn match sloshEscapeSpecial		!#\\[ -}\~]!
syn match sloshEscapeSpecial		!#[':][^()'`,"; \t]\+!
syn match sloshEscapeSpecial		!#([^()'`,"; \t]\+)!
syn match sloshEscapeSpecial		!#\\\%(Space\|Newline\|Tab\|Page\|Rubout\|Linefeed\|Return\|Backspace\)!
syn match sloshEscapeSpecial		"\<+[a-zA-Z_][a-zA-Z_0-9-]*+\>"

syn match sloshConcat		"\s\.\s"
syn match sloshParenError	")"

" ---------------------------------------------------------------------
" Comments: {{{1
syn cluster sloshCommentGroup	contains=sloshTodo,@Spell
syn cluster sloshDocStringGroup	contains=sloshParen0,sloshParen1,sloshParen2,sloshParen3,sloshParen4,sloshParen5,sloshParen6,sloshParen7,sloshParen8,sloshParen9
syn match   sloshComment	";.*$"				contains=@sloshCommentGroup
syn region  sloshCommentRegion	start="#|" end="|#"		contains=sloshCommentRegion,@sloshCommentGroup
"syn region  sloshCommentDocStr	start="#!" end="!#"	contains=sloshComment,sloshCommentRegion,@sloshDocStringGroup,@sloshCommentGroup
syn region  sloshCommentDocStr	start="#%" end="%#"	contains=sloshComment,sloshCommentRegion,@sloshCommentGroup
syn match   sloshTodo		contained	"todo.*$"
syn match   sloshTodo		contained	"TODO.*$"

" ---------------------------------------------------------------------
" Synchronization: {{{1
syn sync lines=100

" ---------------------------------------------------------------------
" Define Highlighting: {{{1
if !exists("skip_slosh_syntax_inits")

  hi def link sloshCommentRegion	sloshComment
  hi def link sloshCommentDocStr	hlDocString
  hi def link sloshAtomNmbr		sloshNumber
  hi def link sloshAtomMark		sloshMark
  hi def link sloshInStringString	sloshString

  hi def link sloshAtom			Identifier
  hi def link sloshAtomBarSymbol	Special
  hi def link sloshBarSymbol		Special
  hi def link sloshComment		Comment
  hi def link sloshConcat		Statement
  hi def link sloshFunc			Statement
  hi def link sloshKey			Type
  hi def link sloshMark			Delimiter
  hi def link sloshNumber		Number
  hi def link sloshParenError		Error
  hi def link sloshEscapeSpecial	Type
  hi def link sloshString		String
  hi def link sloshTodo			Todo
  hi def link sloshVar			Statement

  "hi def hlDocString ctermfg=blue	guifg=darkslateblue
  hi def hlDocString ctermfg=lightgreen	guifg=green1
  if &bg == "dark"
   hi def hlLevel0 ctermfg=red		guifg=red1
   hi def hlLevel1 ctermfg=yellow	guifg=orange1
   hi def hlLevel2 ctermfg=green	guifg=yellow1
   hi def hlLevel3 ctermfg=cyan	guifg=greenyellow
   hi def hlLevel4 ctermfg=magenta	guifg=green1
   hi def hlLevel5 ctermfg=red		guifg=springgreen1
   hi def hlLevel6 ctermfg=yellow	guifg=cyan1
   hi def hlLevel7 ctermfg=green	guifg=slateblue1
   hi def hlLevel8 ctermfg=cyan	guifg=magenta1
   hi def hlLevel9 ctermfg=magenta	guifg=purple1
  else
   hi def hlLevel0 ctermfg=red		guifg=red3
   hi def hlLevel1 ctermfg=darkyellow	guifg=orangered3
   hi def hlLevel2 ctermfg=darkgreen	guifg=orange2
   hi def hlLevel3 ctermfg=blue	guifg=yellow3
   hi def hlLevel4 ctermfg=darkmagenta	guifg=olivedrab4
   hi def hlLevel5 ctermfg=red		guifg=green4
   hi def hlLevel6 ctermfg=darkyellow	guifg=paleturquoise3
   hi def hlLevel7 ctermfg=darkgreen	guifg=deepskyblue4
   hi def hlLevel8 ctermfg=blue	guifg=darkslateblue
   hi def hlLevel9 ctermfg=darkmagenta	guifg=darkviolet
  endif

endif

let b:current_syntax = "slosh"

" ---------------------------------------------------------------------
" vim: ts=4 nowrap fdm=marker
