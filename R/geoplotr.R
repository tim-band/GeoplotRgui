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
      twostage="twoStage",
      kde="kde",
      bw="bandwidth",
      decision="vermeeschPease"
    ),
    optiongroups=c("plot", "decisionLine","labels")
  ),
  AnAbOr=list(
    params=list(
      An="An",
      Ab="Ab",
      Or="Or"
    ),
    optiongroups=c("plot","labels")
  ),
  ATM=list(
    params=list(
      A="A",
      T="T",
      M="M",
      ternary="ternaryLogratio",
      kde="kde",
      bw="bandwidth",
      decision="vermeeschPease"
    ),
    optiongroups=c("plot", "decisionLine","labels")
  ),
  TAS=list(
    params=list(
      Na2O="Na2O",
      K2O="K2O",
      SiO2="SiO2",
      volcanic="volcanic"
    ),
    optiongroups=c("plot","labels")
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
  SrY=list(
    params=list(
      Sr="Sr",
      Y="Y_SrY"
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
  TiV=list(
    params=list(
      Ti="Ti",
      V="V",
      units="tiv_units",
      type="tiv_type",
      ternary="tiv_plot"
    ),
    optiongroups=c("plot","labels","labels")
  ),
  TiZrY=list(
    params=list(
      Ti="Ti",
      Zr="Zr",
      Y="Y",
      units="tizry_units",
      type="tizry_type",
      ternary="tizry_plot"
    ),
    optiongroups=c("plot","labels","labels")
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
      Ti="Ti",
      units="zrti_units",
      type="zrti_type",
      ternary="zrti_plot"
    ),
    optiongroups=c("plot","labels")
  )
)

params <- list(
  # AFM/ATM
  A=list(type="weightCol", data="A"),
  F=list(type="weightCol", data="F"),
  T=list(type="weightCol", data="T"),
  M=list(type="weightCol", data="M"),
  ternaryLogratio=list(type="b", data="true"),
  twoStage=list(type="b", data="false"),
  kde=list(type="b", data="false"),
  bandwidth=list(type="bandwidth", data="bandwidth"),
  vermeeschPease=list(type="b", data="true"),
  # AnAbOr
  An=list(type="weightCol", data="An"),
  Ab=list(type="weightCol", data="Ab"),
  Or=list(type="weightCol", data="Or"),
  # CrY
  Cr=list(type="floatCol", data="Cr"),
  Y=list(type="floatCol", data="Y"),
  # LaYb
  La_n=list(type="floatCol", data="La_n"),
  Yb_n=list(type="floatCol", data="Yb_n"),
  # TAS
  Na2O=list(type="weightCol", data="Na2O"),
  K2O=list(type="weightCol", data="K2O"),
  SiO2=list(type="weightCol", data="SiO2"),
  volcanic=list(type="b", data="true"),
  showlabels=list(type="b", data="true"),
  shortlabels=list(type="b", data="true"),
  # Rb-Ta (Pearce1984)
  Rb=list(type="floatCol", data="Rb"),
  Ta=list(type="floatCol", data="Ta"),
  # SrY
  Sr=list(type="floatCol", data="Sr"),
  Y_SrY=list(type="floatCol", data="Y_SrY"),
  # ThCo
  Co=list(type="floatCol", data="Co"),
  # Th-Nb-La-Yb (Hollocher2012)
  Th=list(type="floatCol", data="Th"),
  Nb=list(type="floatCol", data="Nb"),
  La=list(type="floatCol", data="La"),
  Yb=list(type="floatCol", data="Yb"),
  # Ti-V
  Ti=list(type="proportionCol_TiO2", data="Ti"),
  V=list(type="floatCol", data="V"),
  tiv_units=list(type="subheader", data="tiv_units"),
  tiv_type=list(type="tiv_type", data="tiv_type"),
  tiv_plot=list(type="b", data=FALSE),
  # Ti-Zr-Y
  Zr=list(type="proportionCol_ZrO2", data="Zr"),
  Y=list(type="proportionCol_Y2O3", data="Y"),
  tizry_units=list(type="subheader", data="tizry_units"),
  tizry_type=list(type="tizry_type", data="tizry_type"),
  tizry_plot=list(type="b", data=TRUE),
  # Ti-Zr
  zrti_units=list(type="subheader", data="zrti_units"),
  zrti_type=list(type="zrti_type", data="zrti_type"),
  zrti_plot=list(type="b", data=FALSE)
)

optiongroups <- list(
  plot=list(
    cex=list(type="f"),
    lwd=list(type="u8"),
    bg=list(type="color", initial='#666'),
    pch=list(type="u8"),
    col=list(type="color", initial='#000')
  ),
  decisionLine=list(
    dlwd=list(type="u8", initial=1.5),
    dcol=list(type="color", initial="blue")
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
  proportionCol_ZrO2=list(
    kind="column",
    subtype="f",
    unittype="proportion_ZrO2"
  ),
  proportionCol_Y2O3=list(
    kind="column",
    subtype="f",
    unittype="proportion_Y2O3"
  ),
  proportion_TiO2=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'TiO2'))
  ),
  proportion_ZrO2=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'ZrO2'))
  ),
  proportion_Y2O3=list(
    kind="enum",
    values=c("wt%", "ppm"),
    factors=c(1, GeoplotR::wtpct2ppm(1, 'Y2O3'))
  ),
  weightCol=list(
    kind="column",
    subtype="f"
  ),
  bandwidth=list(
    kind="enum",
    values=c("nrd0", "nrd", "ucv", "bcv", "SJ")
  )
)

examples <- list(
    Na2O=getColumn("Na2O"),
    K2O=getColumn("K2O"),
    SiO2=getColumn("SiO2"),
    Ti=getColumn("TiO2"),
    Zr=getColumn("Zr"),
    Y=getColumn("Y"),
    A=getCathColumn("Na2O") + getCathColumn("K2O"),
    F=getCathColumn("FeOT"),
    T=getCathColumn("TiO2"),
    M=getCathColumn("MgO"),
    An=c(70,75,73),
    Ab=c(20,10,27),
    Or=c(10,15,0),
    Cr=getColumn("Cr"),
    Y=getColumn("Y"),
    Th=getColumn("Th"),
    Nb=getColumn("Nb"),
    La=getColumn("La"),
    Yb=getColumn("Yb"),
    Rb=getColumn("Rb"),
    Ta=getColumn("Ta"),
    Co=getColumn("Co"),
    V=getColumn("V"),
    tizry_units=c("wt%", "ppm", "ppm"),
    tizry_type="LDA",
    zrti_units=c("ppm","wt%"),
    zrti_type="QDA",
    La_n=c(100),
    Yb_n=c(10),
    Sr=c(1000),
    Y_SrY=c(10),
    true=TRUE,
    false= FALSE,
    bandwidth="nrd0"
)

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
#' \code{IsoplotR} package.
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
      NbLaYb = GeoplotR::NbLaYb,
      SrY = GeoplotR::SrY,
      TAS = GeoplotR::TAS,
      ThCo = GeoplotR::ThCo,
      ThNbLaYb = GeoplotR::ThNbLaYb,
      TiV = TiV,
      TiZrY = TiZrY,
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
