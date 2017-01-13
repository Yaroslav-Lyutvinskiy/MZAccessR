setClass( 'ArrayOfArrayOfDouble' ,
          representation(
            .Data = 'list') ,
          contains = c( 'list' ) )
setAs('XMLInternalElementNode', 'ArrayOfArrayOfDouble',
      function (from, to = "ArrayOfArrayOfDouble", strict = TRUE)
        xmlSApply(from, as, "ArrayOfDouble")
)

setClass( 'ArrayOfDouble' ,
          representation(
            .Data = 'numeric') ,
          contains = c( 'numeric' ) )
setAs('XMLInternalElementNode', 'ArrayOfDouble',
      function (from, to = "ArrayOfDouble", strict = TRUE)
        xmlSApply(from, as, "numeric")
)
setAs('character', 'ArrayOfDouble',
      function (from, to = "ArrayOfDouble", strict = TRUE)
        new("ArrayOfDouble", as(from, "character"))
)
setAs('integer', 'ArrayOfDouble',
      function (from, to = "ArrayOfDouble", strict = TRUE)
        new("ArrayOfDouble", as(from, "integer"))
)
setAs('logical', 'ArrayOfDouble',
      function (from, to = "ArrayOfDouble", strict = TRUE)
        new("ArrayOfDouble", as(from, "logical"))
)

setClass( 'ArrayOfString' ,
          representation(
            .Data = 'character') ,
          contains = c( 'character' ) )
setAs('XMLInternalElementNode', 'ArrayOfString',
      function (from, to = "ArrayOfString", strict = TRUE)
        xmlSApply(from, as, "character")
)
setAs('integer', 'ArrayOfString',
      function (from, to = "ArrayOfString", strict = TRUE)
        new("ArrayOfString", as(from, "integer"))
)
setAs('numeric', 'ArrayOfString',
      function (from, to = "ArrayOfString", strict = TRUE)
        new("ArrayOfString", as(from, "numeric"))
)
setAs('logical', 'ArrayOfString',
      function (from, to = "ArrayOfString", strict = TRUE)
        new("ArrayOfString", as(from, "logical"))
)

setClass( 'FileName.Cache' ,
          representation(
            FileName = 'character',
            Cache = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.Cache',
      function (from, to = "FileName.Cache", strict = TRUE)
        coerceListToS4(from, new("FileName.Cache"))
)

setClass( 'FileName.MZLow.MZHigh.RT.Cache.Profile' ,
          representation(
            FileName = 'character',
            MZLow = 'numeric',
            MZHigh = 'numeric',
            RT = 'numeric',
            Cache = 'logical',
            Profile = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.RT.Cache.Profile',
      function (from, to = "FileName.MZLow.MZHigh.RT.Cache.Profile",
                strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.RT.Cache.Profile"))
)

setClass( 'FileName.MZLow.MZHigh.RTLow.RTHigh' ,
          representation(
            FileName = 'ArrayOfString',
            MZLow = 'ArrayOfDouble',
            MZHigh = 'ArrayOfDouble',
            RTLow = 'ArrayOfDouble',
            RTHigh = 'ArrayOfDouble') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.RTLow.RTHigh',
      function (from, to = "FileName.MZLow.MZHigh.RTLow.RTHigh", strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.RTLow.RTHigh"))
)

setClass( 'FileName.MZLow.MZHigh.RTLow.RTHigh.Cache' ,
          representation(
            FileName = 'character',
            MZLow = 'numeric',
            MZHigh = 'numeric',
            RTLow = 'numeric',
            RTHigh = 'numeric',
            Cache = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.RTLow.RTHigh.Cache',
      function (from, to = "FileName.MZLow.MZHigh.RTLow.RTHigh.Cache",
                strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.RTLow.RTHigh.Cache"))
)

setClass( 'FileName.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile' ,
          representation(
            FileName = 'character',
            MZLow = 'numeric',
            MZHigh = 'numeric',
            RTLow = 'numeric',
            RTHigh = 'numeric',
            Cache = 'logical',
            Profile = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile',
      function (from, to = "FileName.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile",
                strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile"))
)

setClass( 'FileName.MZLow.MZHigh.RTLow.RTHigh.Profile' ,
          representation(
            FileName = 'character',
            MZLow = 'numeric',
            MZHigh = 'numeric',
            RTLow = 'numeric',
            RTHigh = 'numeric',
            Profile = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.RTLow.RTHigh.Profile',
      function (from, to = "FileName.MZLow.MZHigh.RTLow.RTHigh.Profile",
                strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.RTLow.RTHigh.Profile"))
)

setClass( 'FileName.MZLow.MZHigh.ScanNumber.Cache.Profile' ,
          representation(
            FileName = 'character',
            MZLow = 'numeric',
            MZHigh = 'numeric',
            ScanNumber = 'integer',
            Cache = 'logical',
            Profile = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.MZLow.MZHigh.ScanNumber.Cache.Profile',
      function (from, to = "FileName.MZLow.MZHigh.ScanNumber.Cache.Profile",
                strict = TRUE)
        coerceListToS4(from, new("FileName.MZLow.MZHigh.ScanNumber.Cache.Profile"))
)

setClass( 'FileName.RT.Cache' ,
          representation(
            FileName = 'character',
            RT = 'numeric',
            Cache = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.RT.Cache',
      function (from, to = "FileName.RT.Cache", strict = TRUE)
        coerceListToS4(from, new("FileName.RT.Cache"))
)

setClass( 'FileName.ScanNumber.Cache' ,
          representation(
            FileName = 'character',
            ScanNumber = 'integer',
            Cache = 'logical') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileName.ScanNumber.Cache',
      function (from, to = "FileName.ScanNumber.Cache", strict = TRUE)
        coerceListToS4(from, new("FileName.ScanNumber.Cache"))
)

setClass( 'GetAreaResult.ErrorMessage' ,
          representation(
            GetAreaResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetAreaResult.ErrorMessage',
      function (from, to = "GetAreaResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetAreaResult.ErrorMessage"))
)

setClass( 'GetAverageSpectrumResult.ErrorMessage' ,
          representation(
            GetAverageSpectrumResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetAverageSpectrumResult.ErrorMessage',
      function (from, to = "GetAverageSpectrumResult.ErrorMessage",
                strict = TRUE)
        coerceListToS4(from, new("GetAverageSpectrumResult.ErrorMessage"))
)

setClass( 'GetChromatogramResult.ErrorMessage' ,
          representation(
            GetChromatogramResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetChromatogramResult.ErrorMessage',
      function (from, to = "GetChromatogramResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetChromatogramResult.ErrorMessage"))
)

setClass( 'GetMassRangeResult.ErrorMessage' ,
          representation(
            GetMassRangeResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetMassRangeResult.ErrorMessage',
      function (from, to = "GetMassRangeResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetMassRangeResult.ErrorMessage"))
)

setClass( 'GetRTFromScanNumberResult.ErrorMessage' ,
          representation(
            GetRTFromScanNumberResult = 'numeric',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetRTFromScanNumberResult.ErrorMessage',
      function (from, to = "GetRTFromScanNumberResult.ErrorMessage",
                strict = TRUE)
        coerceListToS4(from, new("GetRTFromScanNumberResult.ErrorMessage"))
)

setClass( 'GetRTRangeResult.ErrorMessage' ,
          representation(
            GetRTRangeResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetRTRangeResult.ErrorMessage',
      function (from, to = "GetRTRangeResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetRTRangeResult.ErrorMessage"))
)

setClass( 'GetScanNumberFromRTResult.ErrorMessage' ,
          representation(
            GetScanNumberFromRTResult = 'integer',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetScanNumberFromRTResult.ErrorMessage',
      function (from, to = "GetScanNumberFromRTResult.ErrorMessage",
                strict = TRUE)
        coerceListToS4(from, new("GetScanNumberFromRTResult.ErrorMessage"))
)

setClass( 'GetSpectrumbyRTResult.ErrorMessage' ,
          representation(
            GetSpectrumbyRTResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetSpectrumbyRTResult.ErrorMessage',
      function (from, to = "GetSpectrumbyRTResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetSpectrumbyRTResult.ErrorMessage"))
)

setClass( 'GetSpectrumbyScanNumberResult.ErrorMessage' ,
          representation(
            GetSpectrumbyScanNumberResult = 'ArrayOfDouble',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetSpectrumbyScanNumberResult.ErrorMessage',
      function (from, to = "GetSpectrumbyScanNumberResult.ErrorMessage",
                strict = TRUE)
        coerceListToS4(from, new("GetSpectrumbyScanNumberResult.ErrorMessage"))
)


setMethod("toSOAP", c("logical", "XMLInternalElementNode", type = "PrimitiveSOAPType"),
          function(obj, con = xmlOutputBuffer(header=""), type = NULL, literal = FALSE, elementFormQualified = FALSE, ...)
          {
            if (literal){
              if (.Primitive("as.logical")(obj)) {
                newXMLTextNode("true",parent=con)
              }
              else {
                newXMLTextNode("false",parent = con)
              }
            }else{
              if (.Primitive("as.logical")(obj))
                "true"
              else
                "false"
            }
          })

setAs('list', 'ArrayOfDouble',
      function (from, to = "ArrayOfDouble", strict = TRUE)
        new("ArrayOfDouble", as(from, "list")))


#' @export
MZRange <- setClass("MZRange", slots = c(MinMZ="numeric", MaxMZ="numeric"))
#' @export
RTRange <- setClass("RTRange", slots = c(MinRT="numeric", MaxRT="numeric"))
#' @export
LCMSArea <- setClass("LCMSArea", slots = c(RTRange="RTRange", MZRange="MZRange"))

WDSLEnvir = new.env()

#' Setup for Web-server
#'
#' Checks server connection and loads function prototypes.
#' All other functions become working only afrter SetupServer call.
#'
#' @param ServerURL - WDSL page of mzAccess web server
#' @return None
#' @examples
#' SetupServer()
#' @export
SetupServer =
  function(ServerURL = "http://charon.ad.cmm.se:6060/DevRawFileAccess/MSDataService.asmx?WSDL"){
    wsdl = processWSDL(ServerURL)
    assign("wsdl", wsdl, envir = WDSLEnvir)
    iface= genSOAPClientInterface(wsdl)
    assign("iface", iface, envir = WDSLEnvir)
  }

#' Get XIC Chromatogram
#'
#' Gets eXtracted Ion Current (XIC) chromatograms for specified LC-MS area.
#' Corresponds to mzAccess web-service API function GetChromatogram
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - Minimum m/z value for LC-MS area requested
#' @param MZHigh - Maximum m/z value for LC-MS area requested
#' @param RTLow - Minimum retention time value for LC-MS area requested
#' @param RTHigh - Maximum retention time value for LC-MS area requested
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @return Data frame of Retention Time and Intensities for requested LC-MS area
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
#' @import XML
#' @import XMLSchema
#' @import SSOAP
GetChromatogram =
  function(FileName, MZLow, MZHigh, RTLow, RTHigh, Cache=TRUE){
    iface = get("iface", envir = WDSLEnvir)
    CH1=iface@functions$GetChromatogram(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RTLow = RTLow, RTHigh = RTHigh ,
      Cache=Cache))
    if (length(CH1@ErrorMessage)>1) print(CH1@ErrorMessage)
    L=CH1@GetChromatogramResult
    CHR=data.frame(RT=L[c(TRUE,FALSE)],Intensity=L[c(FALSE,TRUE)])
    return(CHR)
  }

#' Get LC-MS Area signals
#'
#' Gets all non-zero signals for specified LC-MS area.
#' Corresponds to mzAccess web-service API function GetArea
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - Minimum m/z value for LC-MS area requested
#' @param MZHigh - Maximum m/z value for LC-MS area requested
#' @param RTLow - Minimum retention time value for LC-MS area requested
#' @param RTHigh - Maximum retention time value for LC-MS area requested
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @return Data frame of Mass, Retention Time and Intensities for requested LC-MS area
#' @examples
#' GetArea(FileName="010_MOO_Labeling_HIL_72h_0_1",149.047,149.074, RTLow = 8.2, RTHigh = 8.5,Profile = TRUE, Cache = FALSE)
#' @export
GetArea =
  function(FileName, MZLow, MZHigh, RTLow, RTHigh, Cache=TRUE, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Ar=iface@functions$GetArea(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RTLow = RTLow, RTHigh = RTHigh ,
      Cache=Cache, Profile = Profile))
    if (length(Ar@ErrorMessage)>1) print(Ar@ErrorMessage)
    L=Ar@GetAreaResult
    AREA=data.frame(Mass=L[c(TRUE,FALSE,FALSE)],RT=L[c(FALSE,TRUE,FALSE)],Intensity=L[c(FALSE,FALSE,TRUE)])
    return(AREA)
  }
#' Get averaged spectrum
#'
#' Get spectrum averaged (or summed) within RT interval
#' Corresponds to mzAccess web-service API function GetAverageSpectrum
#' Averaging algorithm is  vendor specific, therefore function has no cache option
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - Minimum m/z value to be included in spectrum
#' @param MZHigh - Maximum m/z value to be included in spectrum
#' @param RTLow - Minimum retention time value for RT range of averaged spectrum
#' @param RTHigh - Maximum retention time value for RT range of averaged spectrum
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @return Data frame of Mass and Intensities for requested LC-MS area
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetAvgSpectrum =
  function(FileName, MZLow, MZHigh, RTLow, RTHigh, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetAverageSpectrum(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RTLow = RTLow, RTHigh = RTHigh ,
      Profile = Profile))
    if (length(Sp@ErrorMessage)>1) print(Sp@ErrorMessage)
    L=Sp@GetAverageSpectrumResult
    SPC=data.frame(Mass=L[c(TRUE,FALSE)],Intensity=L[c(FALSE,TRUE)])
    return(SPC)
  }

#' Get spectrum for  RT
#'
#' Get Single scan spectrum on certain RT.
#' Web-service will provide first spectrum where RT is not less than specified
#' Corresponds to mzAccess web-service API function GetSpectrumbyRT
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - Minimum m/z value to be included in spectrum
#' @param MZHigh - Maximum m/z value to be included in spectrum
#' @param RT - retention time for which spectrum will be searched.
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @return Data frame of Mass and Intensities for requested LC-MS area
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetSpectrumbyRT =
  function(FileName, MZLow, MZHigh, RT, Cache=TRUE, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetSpectrumbyRT(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RT = RT,
      Cache=Cache, Profile = Profile))
    if (length(Sp@ErrorMessage)>1) print(Sp@ErrorMessage)
    L=Sp@GetSpectrumbyRTResult
    SPC=data.frame(Mass=L[c(TRUE,FALSE)],Intensity=L[c(FALSE,TRUE)])
    return(SPC)
  }

#' Get spectrum for scan number
#'
#' Get Single scan spectrum for certain scan.
#' Corresponds to mzAccess web-service API function GetSpectrumbyScanNumber
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - Minimum m/z value to be included in spectrum
#' @param MZHigh - Maximum m/z value to be included in spectrum
#' @param ScanNumber - scan number for requested spectrum.
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @return Data frame of Mass and Intensities for requested LC-MS area
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetSpectrumbyScanNumber =
  function(FileName, MZLow, MZHigh, ScanNumber, Cache=TRUE, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetSpectrumbyScanNumber(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      ScanNumber = ScanNumber,
      Cache=Cache, Profile = Profile))
    if (length(Sp@ErrorMessage)>1) print(Sp@ErrorMessage)
    L=Sp@GetSpectrumbyScanNumber
    SPC=data.frame(Mass=L[c(TRUE,FALSE)],Intensity=L[c(FALSE,TRUE)])
    return(SPC)
  }

#' Get scan number for retention time
#'
#' Get scan number
#' Corresponds to mzAccess web-service API function GetScanNumberFromRT
#' Web-service will provide first scan number  where RT is not less than specified
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param RT - retention time for which spectrum will be searched.
#' @param Cache - If TRUE scan number will be loaded from fast access cache, if FALSE - from original raw files
#' @return Interger scan number
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetScanNumberFromRT =
  function(FileName, RT, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Scan=iface@functions$GetScanNumberFromRT(list(
      FileName=FileName,
      RT = RT, Cache=Cache))
    if (length(Scan@ErrorMessage)>1) print(Scan@ErrorMessage)
    L=Scan@GetScanNumberFromRTResult
    return(L)
  }

#' Get retention time for scan number
#'
#' Get retention time for scan number
#' Corresponds to mzAccess web-service API function GetRTFromScanNumber
#' Web-service will provide first scan number  where RT is not less than specified
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param RT - retention time for which spectrum will be searched.
#' @param Cache - If TRUE retention time value (in minutes) will be loaded from fast access cache, if FALSE - from original raw files
#' @return Retention time value
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetRTFromScanNumber =
  function(FileName, ScanNumber, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    RT=iface@functions$GetRTFromScanNumber(list(
      FileName=FileName,
      ScanNumber = ScanNumber, Cache=Cache))
    if (length(RT@ErrorMessage)>1) print(RT@ErrorMessage)
    L=RT@GetRTFromScanNumberResult
    return(L)
  }


#' Get mass range for spectra in file
#'
#'  Get mass range for spectra in file
#' Corresponds to mzAccess web-service API function GetMassRange
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param Cache - If TRUE Mass Range will show minimum amd maximim masses avialable in spectra through the file, if FALSE - it will return Mass Interval where masses were scanned by mass spectrometer
#' @return MassRange object with slots MinMZ,MaxMZ
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetMassRange =
  function(FileName, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Range=iface@functions$GetMassRange(
      list(FileName=FileName, Cache=Cache))
    if (length(Range@ErrorMessage)>1) print(Range@ErrorMessage)
    R=MZRange(MinMZ=Range@GetMassRangeResult[1],
              MaxMZ=Range@GetMassRangeResult[2])
    return(R)
  }

#' Get retention time range for spectra in file
#'
#' Get retention time range for spectra in file
#' Corresponds to mzAccess web-service API function GetRTRange
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param Cache - If TRUE RT Range will show RTs for first and last spectra in spectra through the file, if FALSE - it will retention time range as registered by mass spectrometer
#' @return RTRange object with slots MinRT,MaxRT
#' @examples
#' GetChromatogram("031_MOO_Labeling_HIL_72h_100_3d_MM",148.0584342,148.0624342,0,16)
#' @export
GetRTRange =
  function(FileName, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Range=iface@functions$GetRTRange(
      list(FileName=FileName, Cache=Cache))
    if (length(Range@ErrorMessage)>1) print(Range@ErrorMessage)
    R=RTRange(MinRT=Range@GetRTRangeResult[1],
              MaxRT=Range@GetRTRangeResult[2])
    return(R)
  }

