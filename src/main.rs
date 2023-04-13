#![allow(dead_code, unused_variables)]

use clap::{Args, Parser, Subcommand, ValueEnum};
use decoder::{Decoder, DEFAULT_INDENT_LEVEL, INDENT_LEVEL};

mod decoder;

/// Prints the first 10 numbers (1 to 10)
const CODE: &str = "#epyt-gnirts,0dleif,epyt-erudecorp,epyt-lobmys,epyt-rotcev,x,1dleif,epyt-riap,gnirts>-lobmys,esolc,?bir,*,!tes-1dleif,2dleif,?llun,?rotcev,?erudecorp,gnirts>-rebmun,tneitouq,tsil>-rotcev,?lobmys,-,gnirts>-tsil,?gnirts,ton,snoc,htgnel,pool,2gra,tsil-etirw,tsil>-gnirts,xua-gnirts>-rebmun,<,rac,etirw,2rahctup,+,yalpsid,?ecnatsni,?riap,di,?vqe,srahc-etirw,rdc,rahctup,1gra,,,,bir;89k!9,i$89Gl^>?u>FiO!OGliO~Ku^z!Ok!1(_>?^{!*,i$*a@_>?^*a@_>?^>?vS#~OBvS#_*a@_>?^*a@_>?^>?vS#~O^~^BvE^*a@_>?vS;>?vS#~Bt^*a@_>?vS9>?vS#~Bv0^*a@_>?vS5>?vS#~Bu^*a@_>?^~S`J^~D^{!7,i$,i$87@^>IJ^~D^>?vC~D^z!//YC^1vS7vF~YD^2YA^>?vF~YE^*i$M^~T^/YL^~W^(vL>N@^>IJ^>?vK~D^1vLvK~YF^1vS;vF~Bi%^1vS-vF~S^z!2/^(vE>Ai%M^>?vE~T^z!DEl!Aj%!EEo!6j%!>#nQ_^z!=En!5,^5_`~KakRb^GVYIu``vR%YBu^{!C8>Li&^8>RLi&V`kvP~Kk^z>YHki#!LiN!@Em!:,k0Q@_l~D^z!F+i&^z!)iN!3j%!;#k`^{!-Ek!<+i$^z!.8K,i$+bYG^~YJ^zz!Po]&n]#m]$l!Mk!(:lkl!B:lkm!I:lkn!?:lko!0:lkp!4:lkq!+:lkr!H:lks!G:lkt!N:lku]%:lkv.!J:lkv/!K:lkv0!8:lkv1!':lkv2!,:lkv3y";

fn sym_table() {}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Decode the RIBN generated by the Ribbit Compiler
    Decode(DecodeArgs),
}

#[derive(ValueEnum, Clone, Debug)]
enum MetaConfig {
    /// Include all the information about the symbol in the decoded output
    All,

    /// Include the absolute position of the symbol in the ribn
    Pos,
    /// Include the relative position of the symbol in the ribn 
    RelPos,

    /// Include the characters that encode the operation in the ribn
    OpChars,
    /// Include the byte value that encode the operation in the ribn
    OpBytes,

    /// Include the characters of the arguments that encode the operation in the ribn
    OpArgsChars,
    /// Include the byte value of the arguments that encode the operation in the ribn
    OpArgsBytes,
    /// Include the type of the argument of the operation (index or literal)
    OpArgType,
}

#[derive(Args, Debug)]
struct DecodeArgs {
    #[arg(short, long)]
    /// Include the symbol table in the decoded output
    sym_table: bool,

    #[arg(short = 'f', long)]
    /// The file where the decoded output will be written (default is the standard output)
    output_file: Option<String>,

    #[arg(long, default_value_t = DEFAULT_INDENT_LEVEL)]
    /// The indentation level
    indent_level: usize,

    #[arg(short, long, value_enum)]
    meta: Option<Vec<MetaConfig>>,

    //#[arg(default_value = "RR&po-teg,tneve-dnes$,po-tsnoc,eulav@,po-tes,liat,rebmun~eulav:dnib@,esle,$ELUDOM_EVITCAER$,epyt@,tel,epyt-notelgnis,epyt-gnirts,po-llac/pmuj,_,dna,epyt-erudecorp,!tes,niam,enifed,epyt-evitcaer$,$gnidnib-evitcaer$,adbmal,po-fi,ro,fi,renetsil-tneve-dda$,epyt-lobmys,kcehc-ytira-erutaef##,etouq,epyt-rotcev,renetsil-dda$,nigeb,epyt-riap,dnoc,$elbairav-evitcaer$,dneppa-gnirts,>vid<,pmc-gnirts,>p<,tixe,poon-neg,elipmoc,nerdlihc&srtta-tilps,tsil>-rotcev,?rotcev,!tes-1dleif,evitcaer,fer-gnirts,dnibr,rebmun>-gnirts,raac,lave,>tupni<,edon-evresbo,?naeloob,rtta-tes,redner,enilwen,tneitouq,fer-tsil,di-yb-tnemele,xua-pmc-gnirts,xua-rebmun>-gnirts,ngissa-neg,?erudecorp,gnirts>-rebmun,tsil,2xua-rebmun>-gnirts,liat-tsil,xua-gnirtsbus,txet-tes,rdddac,=<,tsil-etirw,2tsil,dnetxe,erudecorp-ekam,tnemele-etaerc,tnenopmoc,xua-gnirts>-rebmun,3tsil,llac-pmoc,*,renni-srtta-tes,?regetni,?tnemele-lmth,dnib-pmoc,etirw,htiw-sdne,edon-lmth-ot,2rahctup,srtta-tes,gnirts-<,yalpsid,?ecnatsni,htiw-strats,gnirts>-tsil,pukool,rddac,fi-dliub,?evitcaer$,llac-neg,htgnel,+,1tsil,?bir,rorre,nigeb-pmoc,ton,tsil>-gnirts,gnirts>-lobmys,0dleif,?llun,1dleif,?lobmys,tneve-dda,<,?=gnirts,nerdlihc-dda,-,sgra-bn-dda,dneppa,2dleif,tes-sj,gnirts>-,gnirtsbus,srahc-etirw,hcae-rof,htgnel-gnirts,edon-dneppa,?gnirts,?lauqe,teg-sj,rddc,rahctup,2gra,rdac,pmoc,esolc,?riap,?vqe,rac,snoc,rdc,1gra,di,,,,bir;8S$liSK!SKHmHmYS8rYS(qX0kwT,iU*wT&fwT)YS6meiT@iU.biTA_iU-YS,m_El9#moX&kyYS.lvK!N8S5ll?mYS#k?mZ3l_?mZ-liTB?mZ-l^}'!S)7$kYS3l^{!S39>mi&:n`kkFniT*_i&{!T*Wm:nkw'kl]?'l_*mZ?ma@l_Bl^~Dl^}'])'l^9)nZ#mlb@l`^'l`~Cm_Bl_~Dl_})]98@mZ%ma_8@mZ%maZ#m`l~i$8@mZ%ma_8@mZ%maZ#m`l~iSA~Z6l_cZ)nka_@lbBla/nZ9ofd@lbAmai$Bl`^~Dl_}+!S4#n`kn8>l^~i$#n`kn8>l^~i$#n`kn8>l^~i$#n`kn8>l^~YOlUl^~Cmw(YGl^~CmkYIl^~YOl^{]K#nYS4la_l}']%#na_k#nk_k~CmiT*_}'!M/nb/nWm:nYMng@lecw(km~Dl@laBl`^})]4/nYMnWm:nh.w1km/nYMnf~CmiT*fdAmaaa^}-!P*mi&^{]@*mYPl`^}']:*mZ@mb`^})]'*mZ:nca_wSD})!/#nb`n94pfAmi&AmbwSOawSO`99oAmfbZ$l`_`~YFl_@l`/nci$/ncZ'nAmJldwS;Am@lGlcwS=BlGla/ncAm@lGlbwS=~CmBlGlbwT(~Dl@la_~CmwS;^/nci$/ncHm^/ncHmZ:nZ'nAmJlgwSEwSOwSOYPlZ@m`wSOwT%~DlJlbGla~Dl@la_~CmwSE^/nci%/ncHm^/ncHmZ'ni$AmJldwSN^~DlJlbGla~Dl@la_~CmwSN^94pfJldGl`Bl_`BlGl`~CmwT%^8Mnc@la_~CmwS=^#nWmZ%mfw.l#nd~YHlbZ>mi&:nYMniT*JleZ?mAmAmfi$i$akZ8mZ$l`mnGl`~CmwSG^/n:nFngZClecFnfZ(ldboGla_~CmwSD^/nZKmdZ)nlbGlbZ(la_~HmCmwSJ_94pfAmi&AmbwSOawSO`99oAmfbZ$l`_`~YFl_@l`/nci$/ncZ'nAmJldwS;Am@lGlcwS=BlGla/ncAm@lGlbwS=~CmBlGlbwT(~Dl@la_~CmwS;^/nci$/ncHm^/ncHmZ:nZ'nAmJlgwSEwSOwSOYPlZ@m`wSOwT%~DlJlbGla~Dl@la_~CmwSE^/nci%/ncHm^/ncHmZ'ni$AmJldwSN^~DlJlbGla~Dl@la_~CmwSN^94pfJldGl`Bl_`BlGl`~CmwT%^8Mnc@la_~CmwS=^#nWmZ%mfw.l#nd~YHlbZ>mi&:nYMniT*JleZ?mAmAmfi$i$akZ8mZ$l`mnGl`~CmwSG^/n:nFngZClecFnfZ(ldboGla_~CmwSD^/nZKmdZ)nlbGlbZ(la_~Hm^~^CmwSL^#ncGlan~CmwS@^Bl_~Dl_#nbZ)nk``m~YFl_})!@'l_#na_n~iSA}'!SFo!T-n!T/m!T+l!SPk]02l_?mIl^}'!S#2luy!:'li$8:ma@l_?mIl^8:ma@l_?mIl^?mIlvS#~HmCmvS#_8:ma@l_?mIl^8:ma@l_?mIl^?mIlvS#~Hm^~^CmvE^8:ma@l_?mIlvS;?mIlvS#~Cmt^8:ma@l_?mIlvS9?mIlvS#~Cmv0^8:ma@l_?mIlvS5?mIlvS#~Cmu^8:ma@l_?mIl^~YLl`Bl^~Dl^}']A'li$'li$9Al@l^?mZ3lBl^~Dl^?mIlvC~Dl^{]-9-lZIl^90mvS7vF~ZJl^93lYS1l^?mIlvF~YS0l^8:mi$YKl^~Ml^9-lYJl^~YFl^2lvL?mZAl@l^?mZ3lBl^?mIlvK~Dl^90mvLvK~YHl^90mvS;vF~Cmi%^90mvS-vF~YLl^{]39-l^2lvE?mQmi%YKl^?mIlvE~Ml^{!9'li$89m@l`^?mX%lBl_~Dl_}']>#nl`^}']JZ,ll!S1iI!S0Z,lo!S99*lVmYKl`YKl^}']E9*la9EoAmfYS-mbb`a_YAml`~YDma_}+!;9Eoi&b`^})]M'lk'liU,~Dl_'ll9Mm@lb@l`'ll~YDm`^'liU,~YDm__Bl`Bl^~Dl_~Dl^}'!S79MmYKl`YKl^}'!C,mkYS7m`^}'!S-9Om`YIl^}'!8iG!KiI]*#nnZ$l_^{!6Z,ln]G'l_'li$'li$9GmYAmYAmvR%`Z8mbu@l_~YDmvR/^~YDm_vR$Bl^~Dl^}']L9Gmk^'li$~YHl^{!S+'li$8Am_k~^ZLl^9Ll@l^~CmvPBl^'li$~YHl^YKl^{];'l^9;m_`~YDmakAmb^Z#mYAmZ8mu``vR%ZPmu^}']I9*lZ;mi&^9*lAmZ;mi&YAm`kvP~YDmk^{]B8LlYDm__}']68LlYOl^{?mYS/mki#!JiG!FZ,lm]F'l^9FmYAml`@l^~YDm`k}']O+lZFm`^}'!?'l_*mVma@l_Bl^~Dl^}']$'lk9#mZ$l@l_l~Dl^{!H,mi&^{!S*8IlYIl^{]C9(lYGl^{](0lYGl^{!38GlYGl^{!08IlYGl^{!)iG!+iI!*#nk`^}'!-Z,lk!5'li$'li$'li$'li$5mYIlaYIl_~LmYGlaYGl_~LmUlaUl_~YOl`'li$~CmpUl_~YOl_'l^~^Cm`^}'!S&8Ll_'l^~^Cmi%^{!L,mi$^{],.l'li$,mbUl^~YOl^{{!T$p!S?o!T#n!SBm!SMl!S<k!=:nlkl!4:nlkm!S':nlkn]5:nlko!7:nlkp]1:nlkq]=:nlkr!E:nlks]D:nlkt!S%:nlku]N:nlkv.!<'liTC9*l^~Dl^'liU+'liT8~^~YS&l^9Il^~Z6l^'l^~Ml^{].'liTP8S)l^~YCmiT;_8CmiU$^~YCmiT<_8S+l^~YCmiU%_'l^~YCmiT6_}'!S27%l`Hm^[$El*m_b7,l_[,VmAmAmi&bRnOlal_fYJl`Jl`Gl_Bl^~i$*m_b7,l_[,VmAmAmi&bRnOlal_fYJl`Jl`Gl_Bl^~i$*m_b7,l_[,VmAmAmi&bRnOlal_fYJl`Jl`Gl_Bl^~YCmiTNRnlkYJlBl^~YFlBl^~Dl^{i$[$i&i${!S(9<m_iT7z!S69<m_iTOz!S89<m_iT:z]<'l^?mi$'l^?mHmHmHmYBm_a?mZ/m`a@l_Bl^YS2l`~Dl`Z=l^}'!B'li$87m`^~Z5l_89m`El8Bm_a{~Dl_87mZ1l`^~HmMl`'li$87m`^~Z5l_89m`El8Bm_a{~Dl_87mZ1l`^~Hm^~^Z6l_87m``?mX)mEl8Bm_c?mZDmiU#c{wS>?mYBm__X&kZ=liT>~i$'li$87m`^~Z5l_89m`El8Bm_a{~Dl_87mZ1l`^~HmMl`'li$87m`^~Z5l_89m`El8Bm_a{~Dl_87mZ1l`^~Hm^~^Z6l_87m``?mX)mEl8Bm_c?mZDmiU#c{wS>?mYBm__X&kZ=liT>~Z&l_~i%}']/'li$9/m`b?mZ7nb_bYJl`9/m`b?mZ7nb_b`~Ml`JlaGl`Bl_9/m`b?mZ7nb_bYJl`9/m`b?mZ7nb_b`~Ml`@la@lBl`YS*l_~DlBl_~Dl_}']78S%nb`^8S'nAmi&`b_RnOlau_~Z+miU&_8NlYS9m_iT=8EnEl7-lKmiU)KmiT9^{iU(_~YCmiT?^'li$7*mEl8=nSl`iT2d{wS>~i$'li$7*mEl8=nSl`iT2d{wS>~Z&lb~i%?mYEnEl7.lZ.mdKmiTLKmiT3^{iTM`iU''li$7*mEl8=nSl`iT2d{wS>~i$'li$7*mEl8=nSl`iT2d{wS>~Z&lb~i%?mYEnEl7.lZ.mdKmiTLKmiT3^{iTM`iT1~Z2miTI^'li$7*mEl8=nSl`iT2d{wS>~i$'li$7*mEl8=nSl`iT2d{wS>~Z&lb~i%?mYEnEl7.lZ.mdKmiTLKmiT3^{iTM`iT4~Z2miTK^'li$7*mEl8=nSl`iT2d{wS>~i$'li$7*mEl8=nSl`iT2d{wS>~Z&lb~i%?mYEnEl7.lZ.mdKmiTLKmiT3^{iTM`iT5~Z2miTJ^'li$7*mEl8=nSl`iT2d{wS>~i$'li$7*mEl8=nSl`iT2d{wS>~Z&lb~i%?mYEnEl7.lZ.mdKmiTLKmiT3^{iTM`iTE~Z2miT0^~Z+miTF^RnOlap_~Z+miU/_8EnEl7,k{__RnOlan_~Z+miTG_8EnbRnOlbr`^~Z+miU0_})]2'li$'li$'li$8CmRnaYAmb`aa~ZBm__Ol_Ol_~Ml_~Ml^}']+'li$'li$'li$8CmaRn`k_~ZBmOl`^Ol_~Ml_~Ml^}']H'l^z!S$87m_ZNliTH{!S,.l8NliU18Nm`iTD'lwSH~LmwSI_'lk[,VmYPlBl_f~LmwS>_@l`^~YFl^Bl^'lb~YHl^z?mPmcEl7&mbwS>{?mi$.l8NliU18Nm`iTD'lwSH~LmwSI_'lk[,VmYPlBl_f~LmwS>_@l`^~YFl^Bl^'lb~YHl^z?mPmcEl7&mbwS>{[(ZHlb~YLlDlbEl89mcEl7%ld{['X)k{X%ki&}'!S..l'li&89mdEl7%la{[*^~YLlLm_d8Nm`iU289mh-El7&mbc{[/_Jl`Gl_Bl^~LmwT._'lwS:~LmwSI_'lk[*VmZHlBl_d~LmwSC_'lk[+VmYPlBl_e~LmwS>_@l`^~YFl^Bl^'lc~YHl^zi&i&{]&9Jl^{!T'i%!SAi%!U*:nnq:nk:nk:nk:nk:nk:nki&vS9vS,vS)vS4vS<vS5!T@:nnp:nk:nk:nk:nk:nki&vCvS:vS0vCvS?!U.:nnl:nki&vR#!TA:nns:nk:nk:nk:nk:nk:nk:nk:nki&vCvNvCvR)vCvS:vS0vC!U-:nnn:nk:nk:nki&vCvR/vS@!TB:nnl:nki&vC!U,YAmlk!TC:nno:nk:nk:nk:nki&vS3vS3vS<vS5!U+:nnp:nk:nk:nk:nk:nki&vS,vS:vS3vS(vS-!T8:nno:nk:nk:nk:nki&vS,vS<vS9vS;!TP:nno:nk:nk:nk:nki&vS3vS3vS<vS5!T;:nnp:nk:nk:nk:nk:nki&vS@vS(vS9vS9vS(!U$:nno:nk:nk:nk:nki&vS,vS<vS9vS;!T<:nnr:nk:nk:nk:nk:nk:nk:nki&vS5vS(vS,vS3vS6vS6vS)!U%:nnq:nk:nk:nk:nk:nk:nki&vS9vS,vS)vS4vS<vS5!T6:nnq:nk:nk:nk:nk:nk:nki&vS.vS5vS0vS9vS;vS:!TN:nnl:nki&vR5!T7:nnp:nk:nk:nk:nk:nki&vS;vS<vS7vS5vS0!TO:nnl:nki&vS7!T::nnn:nk:nk:nki&vS=vS0vS+!U#:nnki&!T>:nno:nk:nk:nk:nki&vS5vS(vS7vS:!U&:nnu:nk:nk:nk:nk:nk:nk:nk:nk:nk:nki&vR/vS9vS;vS;vS(vPvS+vS5vS0vS)!T=:nnv;:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nki&vCvR/vS,vS;vS<vS)vS0vS9vS;vS;vS(vCvS+vS5vS0vS)vCvS5vS>vS6vS5vS2vS5vRJ!U):nnr:nk:nk:nk:nk:nk:nk:nki&vS+vS,vS2vS*vS,vS/vS*!T9:nnq:nk:nk:nk:nk:nk:nki&vS;vS,vS.vS9vS(vS;!U(:nnp:nk:nk:nk:nk:nki&vS;vS<vS7vS5vS0!T?:nnr:nk:nk:nk:nk:nk:nk:nki&vS+vS,vS2vS*vS,vS/vS*!U':nnq:nk:nk:nk:nk:nk:nki&vS.vS5vS0vS9vS;vS:!T1:nnp:nk:nk:nk:nk:nki&vS@vS(vS9vS9vS(!TI:nnq:nk:nk:nk:nk:nk:nki&vS@vS(vS9vS9vS(vSE!T4:nnq:nk:nk:nk:nk:nk:nki&vS.vS5vS0vS9vS;vS:!TK:nnr:nk:nk:nk:nk:nk:nk:nki&vS.vS5vS0vS9vS;vS:vSE!T5:nnr:nk:nk:nk:nk:nk:nk:nki&vS5vS(vS,vS3vS6vS6vS)!TJ:nns:nk:nk:nk:nk:nk:nk:nk:nki&vS5vS(vS,vS3vS6vS6vS)vSE!T2:nnp:nk:nk:nk:nk:nki&vS,vS<vS3vS(vS=!TL:nnp:nk:nk:nk:nk:nki&vS,vS<vS3vS(vS=!T3:nnq:nk:nk:nk:nk:nk:nki&vS;vS,vS.vS9vS(vS;!TM:nnp:nk:nk:nk:nk:nki&vS;vS<vS7vS5vS0!TE:nnq:nk:nk:nk:nk:nk:nki&vS9vS,vS)vS4vS<vS5!T0:nnr:nk:nk:nk:nk:nk:nk:nki&vS9vS,vS)vS4vS<vS5vSE!TF:nnp:nk:nk:nk:nk:nki&vS,vS<vS3vS(vS=!U/:nnp:nk:nk:nk:nk:nki&vR/vS+vS5vS0vS)!TG:nnn:nk:nk:nki&vR/vS5vS6!U0:nnr:nk:nk:nk:nk:nk:nk:nki&vR/vS>vS(vS9vPvS5vS6!TH:nnn:nk:nk:nki&vS7vS7vS(!U1:nnvRP:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nki&vR#vS@vS*vS5vS,vS+vS5vS,vS7vS,vS+vCvS9vS0vS,vS/vS;vCvS-vS6vCvS,vS5vS6vCvCvCttttuvCvS-vS6vCvS,vS<vS3vS(vS=vCvS,vS/vS;vCvS.vS5vS0vS.vS5vS(vS/vS*vCvS@vS)vCvS+vS,vS.vS5vS(vS/vS*vCvS,vS)vCvS@vS3vS5vS6vCvS5vS(vS*vCvS:vS,vS<vS3vS(vS=vCvS+vS,vS+vS5vS0vR7!TD:nnv2:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nki&vCvS+vS6vS/vS;vS,vS4vCvS5vS>vS6vS5vS2vS5vRJ!U2:nnv2:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nk:nki&vCvS+vS6vS/vS;vS,vS4vCvS5vS>vS6vS5vS2vS5vRJ!=:nlkl!4:nlkm!S':nlkn]5:nlko!7:nlkp]1:nlkq]=:nlkr!E:nlks]D:nlkt!S%:nlku]N:nlkv.!S5:nlkv/!2:nlkv0]P:nlkv1]8:nlkv2!A:nlkv3]#:nlkv4!D:nlkv5!,:nlkv6!S/:nlkv7!>:nlkv8!G:nlkv9!I:nlkv:!O:nlkv;!.:nlkv<!1:nlkv=!(:nlkv>!':nlkv?{")]
    //#[arg(default_value = "#1gra,rahctup,,,,;'vD?>vRD?>vRA?>vRA?>vR:?>vR=!(:lkm!':lkv6{")]
    ribn: String,
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

fn decode(ribn: String) {
    let mut decoder = Decoder::new(ribn.clone());
    let (mut symbol_table, symbol_table_str) = decoder.decode_symbol_table();
    let (pc, op_str) = decoder.decode(&mut symbol_table);
    /*
    while pc != NIL {
        println!("{}", op_rib_to_string(&pc));
        if let Obj::Rib(next) = pc.next_op() {
            pc = *next;
        } else {
            // jump!
            println!();
            while let Obj::Number(n) = pc.next_op() {
                if n != 0 { break; }
                pc = pc.get_field1().as_rib().expect("Expected a Rib");
            }
            pc = pc.get_field1().as_rib().expect("Expected a Rib");
        };
    }
    */
    let (sym_table_str, code_str) = ribn.split_once(';').unwrap();
    println!("Ribn:\n{}\n", sym_table_str);
    println!("Symbol Table:");
    symbol_table_str.iter().for_each(|s| println!("{}", s));
    println!("\nCode:\n{}\n\n", code_str);
    op_str.iter().for_each(|s| println!("{}", s))
}

/// Extract the symbol table
///
fn main() {
    let cli = Cli::parse();

    match &cli.command {
        Commands::Decode(DecodeArgs {
            ribn, indent_level, meta, ..
        }) => {
            unsafe {
                INDENT_LEVEL = *indent_level;
            }
            decode(ribn.clone());
        }
    }
}
