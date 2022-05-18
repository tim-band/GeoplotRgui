dataEnvironment <- new.env(parent=emptyenv())

fetchTestData <- function() {
  if (!exists("test", dataEnvironment)) {
    data(test, package='GeoplotR', envir=dataEnvironment);
  }
}

fetchCathData <- function() {
  if (!exists("cath", dataEnvironment)) {
    data(cath, package='GeoplotR', envir=dataEnvironment);
  }
}

getColumn <- function(column) {
  fetchTestData()
  get("test", dataEnvironment)[,column]
}

getCathColumn <- function(column) {
  fetchCathData()
  cath <- get("cath", dataEnvironment)
  cascades <- cath$affinity=='ca'
  cath[[column]][cascades]
}

functions <- list(
  AFM=list(
    params=list(
      A="A",
      F="F",
      M="M",
      ternary="ternaryLogratio",
      twostage="twoStage"
    ),
    optiongroups=c("plot","decisionLine","labels","BF")
  ),
  ATM=list(
    params=list(
      A="A",
      T="T",
      M="M",
      ternary="ternaryLogratio"
    ),
    optiongroups=c("plot","decisionLine","labels","BF")
  ),
  AnAbOr=list(
    params=list(
      An="An",
      Ab="Ab",
      Or="Or"
    ),
    optiongroups=c("plot","labels")
  ),
  cart_all=list(
      params=list(
          SiO2="SiO2", TiO2="TiO2", Al2O3="Al2O3",
          Fe2O3="Fe2O3", FeO="FeO", CaO="CaO", MgO="MgO",
          MnO="MnO", K2O="K2O", Na2O="Na2O", P2O5="P2O5",
          La="La", Ce="Ce", Pr="Pr", Nd="Nd", Sm="Sm",
          Eu="Eu", Gd="Gd", Tb="Tb", Dy="Dy", Ho="Ho",
          Er="Er", Tm="Tm", Yb="Yb", Lu="Lu", Sc="Sc", V="V",
          Cr="Cr", Co="Co", Ni="Ni", Cu="Cu", Zn="Zn",
          Ga="Ga", Rb="Rb", Sr="Sr", Y="Y", Zr="Zr", Nb="Nb",
          Sn="Sn", Cs="Cs", Ba="Ba", Hf="Hf", Ta="Ta",
          Pb="Pb", Th="Th", U="U", NdNd="NdNd",
          SrSr="SrSr", Pb64="Pb64", Pb74="Pb74", Pb84="Pb84"
      ),
      optiongroups=c("plot")
  ),
  cart_HFS=list(
      params=list(
          Ti="Ti_prop", La="La", Ce="Ce", Pr="Pr", Nd="Nd", Sm="Sm",
          Eu="Eu", Gd="Gd", Tb="Tb", Dy="Dy", Ho="Ho",
          Er="Er", Tm="Tm", Yb="Yb", Lu="Lu", Sc="Sc", Y="Y",
          Zr="Zr", Nb="Nb", Hf="Hf", Ta="Ta", Pb="Pb", Th="Th", U="U",
          NdNd="NdNd", SrSr="SrSr", Pb64="Pb64", Pb74="Pb74",
          Pb84="Pb84", units="cart_HFS_units"
      ),
      optiongroups=c("plot")
  ),
  cart_ratios=list(
      params=list(
          Ti="Ti_prop", La="La", Ce="Ce", Nd="Nd", Sm="Sm", Eu="Eu", Gd="Gd",
          Tb="Tb", Dy="Dy", Ho="Ho", Er="Er", Tm="Tm", Yb="Yb", Lu="Lu",
          Sc="Sc", V="V", Sr="Sr", Y="Y", Zr="Zr", Nb="Nb", Hf="Hf", Ta="Ta",
          Th="Th", U="U", units="cart_ratios_units"
      ),
      optiongroups=c("plot")
  ),
  CrY=list(
      params=list(
          Cr="Cr",
          Y="Y"
      ),
      optiongroups=c("plot","labels")
  ),
  LaYb=list(
      params=list(
          La_n="La_n",
          Yb_n="Yb_n"
      ),
      optiongroups=c("plot","labels")
  ),
  NbLaYb=list(
      params=list(
          Nb="Nb",
          La="La",
          Yb="Yb"
      ),
      optiongroups=c("plot","labels")
  ),
  NbZrY=list(
      params=list(
          Nb="Nb",
          Zr="Zr",
          Y="Y",
          type="nbzry_type",
          ternary="nbzry_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  QAPF=list(
      params=list(
          Q="Qtz",
          A="Aspar",
          P="Pspar",
          F="Foid",
          volcanic="volcanic"
      ),
      optiongroups=c("plot","labels")
  ),
  SrY=list(
      params=list(
          Sr="Sr",
          Y="Y_SrY"
      ),
      optiongroups=c("plot","labels")
  ),
  TAS=list(
      params=list(
          Na2O="Na2O",
          K2O="K2O",
          SiO2="SiO2",
          plutonic="plutonic"
      ),
      optiongroups=c("plot","labels")
  ),
  ThCo=list(
      params=list(
          Th="Th",
          Co="Co"
      ),
      optiongroups=c("plot","labels")
  ),
  ThNbLaYb=list(
      params=list(
          Th="Th",
          Nb="Nb",
          La="La",
          Yb="Yb"
      ),
      optiongroups=c("plot","labels")
  ),
  ThTaHf=list(
      params=list(
          Th="Th",
          Ta="Ta",
          Hf="Hf",
          type="thtahf_type",
          ternary="thtahf_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  TiSiSr=list(
      params=list(
          Ti="Ti_prop",
          Si="Si_prop",
          Sr="Sr",
          units="tisisr_units",
          ternary="tisisr_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  LuEuSr=list(
      params=list(
          Lu="Lu",
          Eu="Eu",
          Sr="Sr",
          ternary="lueusr_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  TiVSc=list(
      params=list(
          Ti="Ti_prop",
          V="V",
          Sc="Sc",
          units="tivsc_units",
          ternary="tivsc_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  TiZrYSr=list(
      params=list(
          Ti="Ti_prop",
          Zr="Zr",
          Y="Y",
          Sr="Sr",
          units="tizrysr_units"
      ),
      optiongroups=c("plot","labels")
  ),
  Pearce1976=list(
      params=list(
          SiO2="SiO2",
          Al2O3="Al2O3",
          TiO2="TiO2",
          CaO="CaO",
          MgO="MgO",
          MnO="MnO",
          K2O="K2O",
          Na2O="Na2O"
      ),
      optiongroups=c("plot","labels")
  ),
  NbNaSr=list(
      params=list(
          Nb="Nb",
          Na="Na_prop",
          Sr="Sc",
          units="nbnasr_units",
          ternary="nbnasr_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  TiSmV=list(
      params=list(
          Ti="Ti_prop",
          Sm="Sm",
          V="V",
          units="tismv_units",
          ternary="tismv_ternary"
      ),
      optiongroups=c("plot","labels")      
  ),
  TiV=list(
      params=list(
          Ti="Ti_prop",
          V="V",
          units="tiv_units",
          type="tiv_type",
          ternary="tiv_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  TiZrY=list(
      params=list(
          Ti="Ti_prop",
          Zr="Zr",
          Y="Y",
          units="tizry_units",
          type="tizry_type",
          ternary="tizry_ternary"
      ),
      optiongroups=c("plot","labels")
  ),
  YbTa=list(
      params=list(
          Yb="Yb",
          Ta="Ta"
      ),
      optiongroups=c("plot","labels")
  ),
  YbTaRb=list(
      params=list(
          Yb="Yb",
          Ta="Ta",
          Rb="Rb"
      ),
      optiongroups=c("plot","labels")
  ),
  YNb=list(
      params=list(
          Y="Y",
          Nb="Nb"
      ),
      optiongroups=c("plot","labels")
  ),
  YNbRb=list(
      params=list(
          Y="Y",
          Nb="Nb",
          Rb="Rb"
      ),
      optiongroups=c("plot","labels")
  ),
  ZrTi=list(
      params=list(
          Zr="Zr",
          Ti="Ti_prop",
          units="zrti_units",
          type="zrti_type",
          ternary="zrti_ternary"
      ),
      optiongroups=c("plot","labels")
  )
)

params <- list(
  SiO2=list(type="floatCol", data="SiO2"),
  TiO2=list(type="floatCol", data="TiO2"),
  Al2O3=list(type="floatCol", data="Al2O3"),
  Fe2O3=list(type="floatCol", data="Fe2O3"),
  FeO=list(type="floatCol", data="FeO"),
  CaO=list(type="floatCol", data="CaO"),
  MgO=list(type="floatCol", data="MgO"),
  MnO=list(type="floatCol", data="MnO"),
  K2O=list(type="floatCol", data="K2O"),
  Na2O=list(type="floatCol", data="Na2O"),
  P2O5=list(type="floatCol", data="P2O5"),
  La=list(type="floatCol", data="La"),
  Ce=list(type="floatCol", data="Ce"),
  Pr=list(type="floatCol", data="Pr"),
  Nd=list(type="floatCol", data="Nd"),
  Sm=list(type="floatCol", data="Sm"),
  Eu=list(type="floatCol", data="Eu"),
  Gd=list(type="floatCol", data="Gd"),
  Tb=list(type="floatCol", data="Tb"),
  Dy=list(type="floatCol", data="Dy"),
  Ho=list(type="floatCol", data="Ho"),
  Er=list(type="floatCol", data="Er"),
  Tm=list(type="floatCol", data="Tm"),
  Yb=list(type="floatCol", data="Yb"),
  Lu=list(type="floatCol", data="Lu"),
  Sc=list(type="floatCol", data="Sc"),
  V=list(type="floatCol", data="V"),
  Cr=list(type="floatCol", data="Cr"),
  Co=list(type="floatCol", data="Co"),
  Ni=list(type="floatCol", data="Ni"),
  Cu=list(type="floatCol", data="Cu"),
  Zn=list(type="floatCol", data="Zn"),
  Ga=list(type="floatCol", data="Ga"),
  Rb=list(type="floatCol", data="Rb"),
  Sr=list(type="floatCol", data="Sr"),
  Y=list(type="floatCol", data="Y"),
  Zr=list(type="floatCol", data="Zr"),
  Nb=list(type="floatCol", data="Nb"),
  Sn=list(type="floatCol", data="Sn"),
  Cs=list(type="floatCol", data="Cs"),
  Ba=list(type="floatCol", data="Ba"),
  Hf=list(type="floatCol", data="Hf"),
  Ta=list(type="floatCol", data="Ta"),
  Pb=list(type="floatCol", data="Pb"),
  Th=list(type="floatCol", data="Th"),
  U=list(type="floatCol", data="U"),
  NdNd=list(type="floatCol", data="NdNd"),
  SrSr=list(type="floatCol", data="SrSr"),
  Pb64=list(type="floatCol", data="Pb64"),
  Pb74=list(type="floatCol", data="Pb74"),
  Pb84=list(type="floatCol", data="Pb84"),
  # for Ti-V and Ti-Zr
  Ti_prop=list(type="proportionCol_TiO2", data="Ti_prop"),
  Si_prop=list(type="proportionCol_SiO2", data="Si_prop"),
  Na_prop=list(type="proportionCol_Na2O", data="Na_prop"),
  # AFM/ATM
  A=list(type="floatCol", data="A"),
  F=list(type="floatCol", data="F"),
  T=list(type="floatCol", data="T"),
  M=list(type="floatCol", data="M"),
  ternaryLogratio=list(type="b", data="true"),
  twoStage=list(type="b", data="true"),
  kde=list(type="b", data="false"),
  BF=list(type="b", data="true"),
  # AnAbOr
  An=list(type="floatCol", data="An"),
  Ab=list(type="floatCol", data="Ab"),
  Or=list(type="floatCol", data="Or"),
  # LaYb
  La_n=list(type="floatCol", data="La_n"),
  Yb_n=list(type="floatCol", data="Yb_n"),
  # QAPF
  Qtz=list(type="floatCol", data="Qtz"),
  Aspar=list(type="floatCol", data="Aspar"),
  Pspar=list(type="floatCol", data="Pspar"),
  Foid=list(type="floatCol", data="Foid"),
  volcanic=list(type="b", data="false"),
  # SrY
  Sr_SrY=list(type="floatCol", data="Sr_SrY"),
  Y_SrY=list(type="floatCol", data="Y_SrY"),
  # TAS
  plutonic=list(type="b", data="false"),
  showlabels=list(type="b", data="true"),
  shortlabels=list(type="b", data="true"),
  # Nb-Zr-Y
  nbzry_type=list(type="nbzry_type", data="nbzry_type"),
  nbzry_ternary=list(type="b", data=TRUE),
  # Ti-V
  tiv_units=list(type="subheader", data="tiv_units"),
  tiv_type=list(type="tiv_type", data="tiv_type"),
  tiv_ternary=list(type="b", data=FALSE),
  # Th-Ta-Hf
  thtahf_type=list(type="thtahf_type", data="thtahf_type"),
  thtahf_ternary=list(type="b", data=TRUE),
  # Ti-Zr-Y
  tizry_units=list(type="subheader", data="tizry_units"),
  tizry_type=list(type="tizry_type", data="tizry_type"),
  tizry_ternary=list(type="b", data=TRUE),
  # Ti-Zr
  zrti_units=list(type="subheader", data="zrti_units"),
  zrti_type=list(type="zrti_type", data="zrti_type"),
  zrti_ternary=list(type="b", data=FALSE),
  # Vermeesch (2006)
  tisisr_units=list(type="subheader",data="tisisr_units"),
  tisisr_ternary=list(type="b", data=TRUE),
  tivsc_units=list(type="subheader",data="tivsc_units"),
  tivsc_ternary=list(type="b", data=TRUE),
  nbnasr_units=list(type="subheader",data="nbnasr_units"),
  nbnasr_ternary=list(type="b", data=TRUE),
  tismv_units=list(type="subheader",data="tismv_units"),
  tismv_ternary=list(type="b", data=TRUE),
  lueusr_ternary=list(type="b", data=TRUE),
  tizrysr_units=list(type="subheader",data="tizrysr_units"),
  # cart
  cart_HFS_units=list(type="subheader", data="cart_HFS_units"),
  cart_ratios_units=list(type="subheader", data="cart_ratios_units")
)

optiongroups <- list(
  plot=list(
    cex=list(type="f", initial=1),
    lwd=list(type="u8", initial=1),
    pch=list(type="u8", initial=21)#,
    #bg=list(type="color", initial=NULL),
    #col=list(type="color", initial='#000')
  ),
  decisionLine=list(
    dlwd=list(type="u8", initial=1.5)#,
    #dcol=list(type="color", initial="blue")
  ),
  BF=list(
      kde=list(type="b", initial=TRUE),
      bw=list(type="bandwidth", data="bandwidth"),
      decision=list(type="b", initial=TRUE)
  ),
  labels=list(
    show.labels=list(type="b", initial=TRUE),
    short=list(type="b", initial=TRUE)
  ),
  framework=list(
    autorefresh=list(type="b", initial=FALSE)
  )
)

types <- list(
  tiv_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Shervais")
  ),
  thtahf_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Wood")
  ),
  nbzry_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Meschede")
  ),
  tizry_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Pearce")
  ),
  zrti_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Pearce","Dilek")
  ),
  floatCol=list(
    kind="column",
    subtype="f"
  ),
  proportionCol_TiO2=list(
    kind="column",
    subtype="f",
    unittype="proportion_TiO2"
  ),
  proportion_TiO2=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'TiO2'))
  ),
  proportionCol_SiO2=list(
    kind="column",
    subtype="f",
    unittype="proportion_SiO2"
  ),
  proportion_SiO2=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'SiO2'))
  ),
  proportionCol_Na2O=list(
    kind="column",
    subtype="f",
    unittype="proportion_Na2O"
  ),
  proportion_Na2O=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'Na2O'))
  ),
  bandwidth=list(
    kind="enum",
    values=c("nrd0", "nrd", "ucv", "bcv", "SJ")
  )
)

examples <- list(
    SiO2=getColumn("SiO2"),
    TiO2=getColumn("TiO2"),
    Al2O3=getColumn("Al2O3"),
    Fe2O3=getColumn("Fe2O3"),
    FeO=getColumn("FeO"),
    CaO=getColumn("CaO"),
    MgO=getColumn("MgO"),
    MnO=getColumn("MnO"),
    K2O=getColumn("K2O"),
    Na2O=getColumn("Na2O"),
    P2O5=getColumn("P2O5"),
    La=getColumn("La"),
    Ce=getColumn("Ce"),
    Pr=getColumn("Pr"),
    Nd=getColumn("Nd"),
    Sm=getColumn("Sm"),
    Eu=getColumn("Eu"),
    Gd=getColumn("Gd"),
    Tb=getColumn("Tb"),
    Dy=getColumn("Dy"),
    Ho=getColumn("Ho"),
    Er=getColumn("Er"),
    Tm=getColumn("Tm"),
    Yb=getColumn("Yb"),
    Lu=getColumn("Lu"),
    Sc=getColumn("Sc"),
    V=getColumn("V"),
    Cr=getColumn("Cr"),
    Co=getColumn("Co"),
    Ni=getColumn("Ni"),
    Cu=getColumn("Cu"),
    Zn=getColumn("Zn"),
    Ga=getColumn("Ga"),
    Rb=getColumn("Rb"),
    Sr=getColumn("Sr"),
    Y=getColumn("Y"),
    Zr=getColumn("Zr"),
    Nb=getColumn("Nb"),
    Sn=getColumn("Sn"),
    Cs=getColumn("Cs"),
    Ba=getColumn("Ba"),
    Hf=getColumn("Hf"),
    Ta=getColumn("Ta"),
    Pb=getColumn("Pb"),
    Th=getColumn("Th"),
    U=getColumn("U"),
    Ti_prop=getColumn("TiO2"),
    Si_prop=getColumn("SiO2"),
    Na_prop=getColumn("Na2O"),
    NdNd=getColumn("Nd143/Nd144"),
    SrSr=getColumn("Sr87/Sr86"),
    Pb64=getColumn("Pb206/Pb204"),
    Pb74=getColumn("Pb207/Pb204"),
    Pb84=getColumn("Pb208/Pb204"),
    A=getCathColumn("Na2O") + getCathColumn("K2O"),
    F=getCathColumn("FeOT"),
    T=getCathColumn("TiO2"),
    M=getCathColumn("MgO"),
    An=c(70,75,73),
    Ab=c(20,10,27),
    Or=c(10,15,0),
    Qtz=c(50,40,0,0,80,60),
    Aspar=c(10,20,50,0,0,20),
    Pspar=c(40,40,50,20,20,20),
    Foid=c(0,0,0,80,0,0),
    La_n=c(100),
    Yb_n=c(10),
    Sr_SrY=c(1000),
    Y_SrY=c(10),
    thtahf_type="Wood",
    tiv_type="Shervais",
    tiv_units=c("wt%","ppm"),
    nbzry_type="Meschede",
    tizry_units=c("wt%","ppm","ppm"),
    tizry_type="Pearce",
    zrti_units=c("ppm","wt%"),
    zrti_type="Pearce",
    tisisr_units=c("wt%","wt%","ppm"),
    tivsc_units=c("wt%","ppm","ppm"),
    nbnasr_units=c("ppm","wt%","ppm"),
    tismv_units=c("wt%","ppm","ppm"),
    tizrysr_units=c("wt%","ppm","ppm","ppm"),
    cart_HFS_units=c("ppm"),
    cart_ratios_units=c("wt%"),
    true=TRUE,
    false= FALSE,
    bandwidth="nrd0"
)

Pearce1976 <- function(SiO2,Al2O3,TiO2,CaO,MgO,MnO,K2O,Na2O, ...) {
  major <- data.frame(SiO2,Al2O3,TiO2,CaO,MgO,MnO,K2O,Na2O)
  GeoplotR::Pearce1976(major, ...)
}
TiV <- function(Ti, V, units, ...) {
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  GeoplotR::TiV(Ti, V, ...)
}
TiZrY <- function(Ti, Zr, Y, units, ...) {
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  if (units[[2]] == "wt%") {
    Zr <- GeoplotR::wtpct2ppm(Zr, "ZrO2")
  }
  if (units[[3]] == "wt%") {
    Y <- GeoplotR::wtpct2ppm(Y, "Y2O3")
  }
  GeoplotR::TiZrY(Ti, Zr, Y, ...)
}
TiZrYSr <- function(Ti, Zr, Y, Sr, units, ...) {
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  GeoplotR::TiZrYSr(Ti, Zr, Y, Sr, ...)
}
TiSiSr <- function(Ti, Si, Sr, units, ...){
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  if (units[[2]] == "wt%") {
    Si <- GeoplotR::wtpct2ppm(Si, "SiO2")
  }
  GeoplotR::TiSiSr(Ti, Si, Sr, ...)
}
TiVSc <- function(Ti, V, Sc, units, ...){
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  GeoplotR::TiVSc(Ti, V, Sc, ...)
}
NbNaSr <- function(Nb, Na, Sr, units, ...){
  if (units[[2]] == "wt%") {
    Na <- GeoplotR::wtpct2ppm(Na, "Na2O")
  }
  GeoplotR::NbNaSr(Nb, Na, Sr, ...)
}
TiSmV <- function(Ti, Sm, V, units, ...){
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  GeoplotR::TiSmV(Ti, Sm, V, ...)
}

cart_all <- function(SiO2,TiO2,Al2O3,Fe2O3,FeO,CaO,MgO,
                     MnO,K2O,Na2O,P2O5,La,Ce,Pr,Nd,Sm,
                     Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Sc,V,
                     Cr,Co,Ni,Cu,Zn,Ga,Rb,Sr,Y,Zr,Nb,
                     Sn,Cs,Ba,Hf,Ta,Pb,Th,U,NdNd,
                     SrSr,Pb64,Pb74,Pb84,...){
  dat <- data.frame(SiO2=SiO2, TiO2=TiO2, Al2O3=Al2O3,
                    Fe2O3=Fe2O3, FeO=FeO, CaO=CaO, MgO=MgO,
                    MnO=MnO, K2O=K2O, Na2O=Na2O, P2O5=P2O5,
                    La=La, Ce=Ce, Pr=Pr, Nd=Nd, Sm=Sm,
                    Eu=Eu, Gd=Gd, Tb=Tb, Dy=Dy, Ho=Ho,
                    Er=Er, Tm=Tm, Yb=Yb, Lu=Lu, Sc=Sc, V=V,
                    Cr=Cr, Co=Co, Ni=Ni, Cu=Cu, Zn=Zn,
                    Ga=Ga, Rb=Rb, Sr=Sr, Y=Y, Zr=Zr, Nb=Nb,
                    Sn=Sn, Cs=Cs, Ba=Ba, Hf=Hf, Ta=Ta,
                    Pb=Pb, Th=Th, U=U, 'Nd143/Nd144'=NdNd,
                    'Sr87/Sr86'=SrSr, 'Pb206/Pb204'=Pb64,
                    'Pb207/Pb204'=Pb74, 'Pb208/Pb204'=Pb84,
                    check.names=FALSE)
  GeoplotR::cart(dat,option=1,plot=TRUE,...)
}
cart_HFS <- function(Ti,La,Ce,Pr,Nd,Sm,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,Sc,Y,Zr,
                     Nb,Hf,Ta,Pb,Th,U,NdNd,SrSr,Pb64,Pb74,Pb84,units,...){
  if (units[[1]] == "ppm") {
    TiO2 <- GeoplotR::ppm2wtpct(Ti, "TiO2")
  }
  dat <- data.frame(TiO2=TiO2,La=La,Ce=Ce,Pr=Pr,Nd=Nd,Sm=Sm,Gd=Gd,
                    Tb=Tb,Dy=Dy,Ho=Ho,Er=Er,Tm=Tm,Yb=Yb,Lu=Lu,Sc=Sc,
                    Y=Y,Zr=Zr,Nb=Nb,Hf=Hf,Ta=Ta,Pb=Pb,Th=Th,U=U,
                    "Nd143/Nd144"=NdNd,"Sr87/Sr86"=SrSr,
                    "Pb206/Pb204"=Pb64,"Pb207/Pb204"=Pb74,
                    "Pb208/Pb204"=Pb84,check.names=FALSE)
  GeoplotR::cart(dat,option=2,plot=TRUE,...)
}
cart_ratios <- function(Ti,La,Ce,Nd,Sm,Eu,Gd,Tb,Dy,Ho,Er,Tm,Yb,Lu,
                        Sc,V,Sr,Y,Zr,Nb,Hf,Ta,Th,U,units,...){
  if (units[[1]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  dat <- data.frame(Ti=Ti,La=La,Ce=Ce,Nd=Nd,Sm=Sm,Eu=Eu,Gd=Gd,Tb=Tb,
                    Dy=Dy,Ho=Ho,Er=Er,Tm=Tm,Yb=Yb,Lu=Lu,Sc=Sc,V=V,
                    Sr=Sr,Y=Y,Zr=Zr,Nb=Nb,Hf=Hf,Ta=Ta,Th=Th,U=U)
  GeoplotR::cart(dat,option=3,plot=TRUE,...)
}
ZrTi <- function(Zr, Ti, units, ...) {
  if (units[[1]] == "wt%") {
    Zr <- GeoplotR::wtpct2ppm(Zr, "ZrO2")
  }
  if (units[[2]] == "wt%") {
    Ti <- GeoplotR::wtpct2ppm(Ti, "TiO2")
  }
  GeoplotR::ZrTi(Zr, Ti, ...)
}

#' Starts the \code{GeoplotR} GUI
#'
#' Opens a web-browser with a Graphical User Interface (GUI) for the
#' \code{GeoplotR} package.
#' @param host IP address to listen on, default is 0.0.0.0 (all interfaces)
#' @param port Internet port of the virtual server. If not defined, a
#' random free port will be chosen and the browser will be opened
#' to show the GUI.
#' @param daemonize If TRUE, keep serving forever without returning.
#' This is useful when called from RScript, to keep
#' @return server object, unless daemonize is TRUE.
#' @examples
#' \dontrun{
#' GeoplotR()
#' }
#' \dontrun{
#' GeoplotR(host='127.0.0.1', port=3820, daemonize=TRUE)
#' }
#' @export
GeoplotR <- function(host='0.0.0.0', port=NULL, daemonize=FALSE) {
  appDir <- system.file("www", package = "GeoplotRgui")
  if (appDir == "") {
    stop("Could not find www directory. Try re-installing `GeoplotRgui`.",
      call. = FALSE)
  }
  shinylight::slServer(host=host, port=port, appDir=appDir, daemonize=daemonize,
    interface=list(
      AFM = GeoplotR::AFM,
      AnAbOr = GeoplotR::AnAbOr,
      ATM = GeoplotR::ATM,
      CrY = GeoplotR::CrY,
      LaYb = GeoplotR::LaYb,
      LuEuSr = GeoplotR::LuEuSr,
      NbLaYb = GeoplotR::NbLaYb,
      NbNaSr = NbNaSr,
      NbZrY = GeoplotR::NbZrY,
      Pearce1976 = Pearce1976,
      QAPF = GeoplotR::QAPF,
      SrY = GeoplotR::SrY,
      TAS = GeoplotR::TAS,
      ThCo = GeoplotR::ThCo,
      ThNbLaYb = GeoplotR::ThNbLaYb,
      ThTaHf = GeoplotR::ThTaHf,
      TiSiSr = TiSiSr,
      TiSmV = TiSmV,
      TiV = TiV,
      TiVSc = TiVSc,
      TiZrY = TiZrY,
      TiZrYSr = TiZrYSr,
      cart_all = cart_all,
      cart_HFS = cart_HFS,
      cart_ratios = cart_ratios,
      YbTa = GeoplotR::YbTa,
      YbTaRb = GeoplotR::YbTaRb,
      YNb = GeoplotR::YNb,
      YNbRb = GeoplotR::YNbRb,
      ZrTi = ZrTi,
      getSchema = function() {
        list(functions=functions, params=params, types=types,
          data=examples, optiongroups=optiongroups)
      }
    )
  )
}
