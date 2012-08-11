set tabstop=4 softtabstop=4 shiftwidth=4 noexpandtab
set number
set cindent
set cc=80
"syntax enable
set background=dark
colorscheme mustang
cmap W w
"set langmap='q,\\,w,.e,pr,yt,fy,gu,ci,ro,lp,/[,=],aa,os,ed,uf,ig,dh,hj,tk,nl,s\\;,-',\\;z,qx,jc,kv,xb,bn,mm,w\\,,v.,z/,[-,]=,\"Q,<W,>E,PR,YT,FY,GU,CI,RO,LP,?{,+},AA,OS,ED,UF,IG,DH,HJ,TK,NL,S:,_\",:Z,QX,JC,KV,XB,BN,MM,W<,V>,Z?

call pathogen#infect()
syntax on
filetype plugin indent on

" Powerline
set laststatus=2
let g:Powerline_symbols='compatible'

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
