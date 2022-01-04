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
  TiZrY=list(
    params=list(
      Ti="Ti",
      Zr="Zr",
      Y="Y",
      units="tizry_units",
      type="tizry_type",
      ternary="tizry_plot"
    ),
    optiongroups=c("plot")
  ),
  TAS=list(
    params=list(
      Na2O="Na2O",
      K2O="K2O",
      SiO2="SiO2",
      volcanic="volcanic"
    ),
    optiongroups=c("plot")
  ),
    optiongroups=c("plot", "decisionLine")
  ),
  AnAbOr=list(
    params=list(
      An="An",
      Ab="Ab",
      Or="Or"
    ),
    optiongroups=c("plot")
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
    optiongroups=c("plot", "decisionLine")
  ),
  CrY=list(
    params=list(
      Cr="Cr",
      Y="Y"
    ),
    optiongroups=c("plot")
  ),
  NbLaYb=list(
    params=list(
      Nb="Nb",
      La="La",
      Yb="Yb"
    ),
    optiongroups=c("plot")
  ),
  ThNbLaYb=list(
    params=list(
      Th="Th",
      Nb="Nb",
      La="La",
      Yb="Yb"
    ),
    optiongroups=c("plot")
  ),
  LaYb=list(
    params=list(
      La_n="La_n",
      Yb_n="Yb_n"
    ),
    optiongroups=c("plot")
  ),
  YNb=list(
    params=list(
      Y="Y",
      Nb="Nb"
    ),
    optiongroups=c("plot")
  ),
  YNbRb=list(
    params=list(
      Y="Y",
      Nb="Nb",
      Rb="Rb"
    ),
    optiongroups=c("plot")
  ),
  YbTa=list(
    params=list(
      Yb="Yb",
      Ta="Ta"
    ),
    optiongroups=c("plot")
  ),
  YbTaRb=list(
    params=list(
      Yb="Yb",
      Ta="Ta",
      Rb="Rb"
    ),
    optiongroups=c("plot")
  ),
  SrY=list(
    params=list(
      Sr="Sr",
      Y="Y_SrY"
    ),
    optiongroups=c("plot")
  ),
  ThCo=list(
    params=list(
      Th="Th",
      Co="Co"
    ),
    optiongroups=c("plot")
  ),
  ZrTi=list(
    params=list(
      Zr="Zr",
      Ti="Ti"
    ),
    optiongroups=c("plot")
  )
)

params <- list(
  # TAS
  Na2O=list(type="weightCol", data="Na2O"),
  K2O=list(type="weightCol", data="K2O"),
  SiO2=list(type="weightCol", data="SiO2"),
  volcanic=list(type="b", data="true"),
  # TiZrY
  Ti=list(type="proportionCol_TiO2", data="Ti"),
  Zr=list(type="proportionCol_ZrO2", data="Zr"),
  Y=list(type="proportionCol_Y2O3", data="Y"),
  tizry_units=list(type="subheader", data="tizry_units"),
  tizry_type=list(type="tizry_type", data="tizry_type"),
  tizry_plot=list(type="b", data=TRUE),
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
  # Hollocher2012
  Th=list(type="floatCol", data="Th"),
  Nb=list(type="floatCol", data="Nb"),
  La=list(type="floatCol", data="La"),
  Yb=list(type="floatCol", data="Yb"),
  # LaYb
  La_n=list(type="floatCol", data="La_n"),
  Yb_n=list(type="floatCol", data="Yb_n"),
  # Pearce1984
  Rb=list(type="floatCol", data="Rb"),
  Ta=list(type="floatCol", data="Ta"),
  # SrY
  Sr=list(type="floatCol", data="Sr"),
  Y_SrY=list(type="floatCol", data="Y_SrY"),
  # ThCo
  Co=list(type="floatCol", data="Co")
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
  framework=list(
    autorefresh=list(type="b", initial=FALSE)
  )
)

types <- list(
  tizry_type=list(
    kind="enum",
    values=c("LDA", "QDA", "Pearce")
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
  tizry_units=c("wt%", "ppm", "ppm"),
  tizry_type="LDA",
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
  La_n=c(100),
  Yb_n=c(10),
  Sr=c(1000),
  Y_SrY=c(10),
  true=TRUE,
  false= FALSE,
  bandwidth="nrd0"
)

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
      TiZrY = TiZrY,
      TAS = GeoplotR::TAS,
      ATM = GeoplotR::ATM,
      AnAbOr = GeoplotR::AnAbOr,
      CrY = GeoplotR::CrY,
      NbLaYb = GeoplotR::NbLaYb,
      ThNbLaYb = GeoplotR::ThNbLaYb,
      LaYb = GeoplotR::LaYb,
      YNb = GeoplotR::YNb,
      YNbRb = GeoplotR::YNbRb,
      YbTa = GeoplotR::YbTa,
      YbTaRb = GeoplotR::YbTaRb,
      SrY = GeoplotR::SrY,
      ThCo = GeoplotR::ThCo,
      ZrTi = GeoplotR::ZrTi,
      getSchema = function() {
        list(functions=functions, params=params, types=types,
          data=examples, optiongroups=optiongroups)
      }
    )
  )
}
