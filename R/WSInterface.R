
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

setClass( 'ArrayOfFragmentationInfo' ,
	   representation(
		.Data = 'list') ,
	   contains = c( 'list' ) )
setAs('XMLInternalElementNode', 'ArrayOfFragmentationInfo',
    function (from, to = "ArrayOfFragmentationInfo", strict = TRUE)
        xmlSApply(from, as, "FragmentationInfo")
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

setClass( 'FileListResult.ErrorMessage' ,
          representation(
            FileListResult = 'ArrayOfString',
            ErrorMessage = 'character') ,
          contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileListResult.ErrorMessage',
      function (from, to = "FileListResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("FileListResult.ErrorMessage"))
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
		FileName = 'character',
		MZLow = 'numeric',
		MZHigh = 'numeric',
		RTLow = 'numeric',
		RTHigh = 'numeric') ,
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

setClass( 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache' ,
	   representation(
		FileNames = 'ArrayOfString',
		MZLow = 'ArrayOfDouble',
		MZHigh = 'ArrayOfDouble',
		RTLow = 'ArrayOfDouble',
		RTHigh = 'ArrayOfDouble',
		Cache = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache',
    function (from, to = "FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache", strict = TRUE)
        coerceListToS4(from, new("FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache"))
)

setClass( 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile' ,
	   representation(
		FileNames = 'ArrayOfString',
		MZLow = 'ArrayOfDouble',
		MZHigh = 'ArrayOfDouble',
		RTLow = 'ArrayOfDouble',
		RTHigh = 'ArrayOfDouble',
		Cache = 'logical',
		Profile = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile',
    function (from, to = "FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile", strict = TRUE)
        coerceListToS4(from, new("FileNames.MZLow.MZHigh.RTLow.RTHigh.Cache.Profile"))
)

setClass( 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Profile' ,
	   representation(
		FileNames = 'ArrayOfString',
		MZLow = 'ArrayOfDouble',
		MZHigh = 'ArrayOfDouble',
		RTLow = 'ArrayOfDouble',
		RTHigh = 'ArrayOfDouble',
		Profile = 'logical') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FileNames.MZLow.MZHigh.RTLow.RTHigh.Profile',
    function (from, to = "FileNames.MZLow.MZHigh.RTLow.RTHigh.Profile", strict = TRUE)
        coerceListToS4(from, new("FileNames.MZLow.MZHigh.RTLow.RTHigh.Profile"))
)

setClass( 'FragmentationInfo' ,
	   representation(
		RT = 'numeric',
		ParentMZ = 'numeric',
		ScanNumber = 'integer',
		MSOrder = 'integer',
		Description = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'FragmentationInfo',
    function (from, to = "FragmentationInfo", strict = TRUE)
        coerceListToS4(from, new("FragmentationInfo"))
)

setClass( 'GetAreaArrayResult.ErrorMessage' ,
	   representation(
		GetAreaArrayResult = 'ArrayOfArrayOfDouble',
		ErrorMessage = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetAreaArrayResult.ErrorMessage',
    function (from, to = "GetAreaArrayResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetAreaArrayResult.ErrorMessage"))
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
      function (from, to = "GetAverageSpectrumResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetAverageSpectrumResult.ErrorMessage"))
)

setClass( 'GetChromatogramArrayResult.ErrorMessage' ,
	   representation(
		GetChromatogramArrayResult = 'ArrayOfArrayOfDouble',
		ErrorMessage = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetChromatogramArrayResult.ErrorMessage',
    function (from, to = "GetChromatogramArrayResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetChromatogramArrayResult.ErrorMessage"))
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

setClass( 'GetFragmentationEventsResult.ErrorMessage' ,
	   representation(
		GetFragmentationEventsResult = 'ArrayOfFragmentationInfo',
		ErrorMessage = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetFragmentationEventsResult.ErrorMessage',
    function (from, to = "GetFragmentationEventsResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetFragmentationEventsResult.ErrorMessage"))
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

setClass( 'GetSpectraArrayResult.ErrorMessage' ,
	   representation(
		GetSpectraArrayResult = 'ArrayOfArrayOfDouble',
		ErrorMessage = 'character') ,
	   contains = c( 'VirtualSOAPClass' ) )
setAs('list', 'GetSpectraArrayResult.ErrorMessage',
    function (from, to = "GetSpectraArrayResult.ErrorMessage", strict = TRUE)
        coerceListToS4(from, new("GetSpectraArrayResult.ErrorMessage"))
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

setAs('list', 'ArrayOfString',
      function (from, to = "ArrayOfString", strict = TRUE)
        new("ArrayOfString", as(from, "list")))

setAs('matrix', 'ArrayOfArrayOfDouble',
      function (from, to = "ArrayOfArrayOfDouble", strict = TRUE){
        Res = new("ArrayOfArrayOfDouble")
        for(i in 1:dim(from)[2]){
          Res[[i]]=from[,i]
        }
        Res
     })


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
  function(ServerURL = "http://charon.ad.cmm.se:6060/MzAccessPublicTest/Service.asmx?WSDL"){
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
    if (is.character(CH1@ErrorMessage)) print(CH1@ErrorMessage)
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
    if (is.character(Ar@ErrorMessage)) print(Ar@ErrorMessage)
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
#' GetAvgSpectrum("031_MOO_Labeling_HIL_72h_100_3d_MM", MZLow = 50, MZHigh = 400, RTLow = 6, RTHigh = 8, Profile = FALSE)
#' @export
GetAvgSpectrum =
  function(FileName, MZLow, MZHigh, RTLow, RTHigh, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetAverageSpectrum(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RTLow = RTLow, RTHigh = RTHigh ,
      Profile = Profile))
    if (is.character(Sp@ErrorMessage)) print(Sp@ErrorMessage)
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
#' GetSpectrumbyRT("031_MOO_Labeling_HIL_72h_100_3d_MM", MZLow = 50, MZHigh = 400, RT = 6, Cache = FALSE, Profile = FALSE)
#' @export
GetSpectrumByRT =
  function(FileName, MZLow, MZHigh, RT, Cache=FALSE, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetSpectrumByRT(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      RT = RT,
      Cache=Cache, Profile = Profile))
    if (is.character(Sp@ErrorMessage)) print(Sp@ErrorMessage)
    L=Sp@GetSpectrumByRTResult
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
#' GetSpectrumbyScanNumber("031_MOO_Labeling_HIL_72h_100_3d_MM", MZLow = 50, MZHigh = 400, ScanNumber = 1000, Profile = TRUE)
#' @export
GetSpectrumByScanNumber =
  function(FileName, MZLow, MZHigh, ScanNumber, Cache=FALSE, Profile = FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Sp=iface@functions$GetSpectrumByScanNumber(list(
      FileName=FileName,
      MZLow=MZLow, MZHigh = MZHigh,
      ScanNumber = ScanNumber,
      Cache=Cache, Profile = Profile))
    if (is.character(Sp@ErrorMessage)) print(Sp@ErrorMessage)
    L=Sp@GetSpectrumByScanNumberResult
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
#' GetScanNumberFromRT("031_MOO_Labeling_HIL_72h_100_3d_MM",6)
#' @export
GetScanNumberFromRT =
  function(FileName, RT, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Scan=iface@functions$GetScanNumberFromRT(list(
      FileName=FileName,
      RT = RT, Cache=Cache))
    if (is.character(Scan@ErrorMessage)) print(Scan@ErrorMessage)
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
#' GetRTFromScanNumber("031_MOO_Labeling_HIL_72h_100_3d_MM",1000)
#' @export
GetRTFromScanNumber =
  function(FileName, ScanNumber, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    RT=iface@functions$GetRTFromScanNumber(list(
      FileName=FileName,
      ScanNumber = ScanNumber, Cache=Cache))
    if (is.character(RT@ErrorMessage)) print(RT@ErrorMessage)
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
#' GetMassRange("031_MOO_Labeling_HIL_72h_100_3d_MM")
#' @export
GetMZRange =
  function(FileName, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Range=iface@functions$GetMZRange(
      list(FileName=FileName, Cache=Cache))
    if (is.character(Range@ErrorMessage)) print(Range@ErrorMessage)
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
#' GetRTRange("031_MOO_Labeling_HIL_72h_100_3d_MM")
#' @export
GetRTRange =
  function(FileName, Cache=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    Range=iface@functions$GetRTRange(
      list(FileName=FileName, Cache=Cache))
    if (is.character(Range@ErrorMessage)) print(Range@ErrorMessage)
    R=RTRange(MinRT=Range@GetRTRangeResult[1],
              MaxRT=Range@GetRTRangeResult[2])
    return(R)
  }


#' Get list of avialable files
#'
#' Get list of avialable files by provided mask
#' Corresponds to mzAccess web-service API function FileList
#'
#' @param FileMask - File mask for file search Applied to file name only without extension. Case insensitive. Can contain "*"and "?" mask symbols
#' @return List of available file names
#' @examples
#' FileList("*72h*")
#' @export
FileList =
  function(FileMask = "*"){
    iface = get("iface", envir = WDSLEnvir)
    FList =iface@functions$FileList(FileMask)
    if (is.character(FList@ErrorMessage)) print(FList@ErrorMessage)
    L = FList@FileListResult@.Data
    return(L)
  }


#' Get data frame of fragmentation events in requested LC-MS area
#'
#' Get data frame of fragmentation events in requested LC-MS area
#' Corresponds to mzAccess web-service API function GetFragmentationEvents
#'
#' @param FileName - Name of original raw mass spectrometry file.
#' @param MZLow - Minimum parent ion m/z value to be included in event list
#' @param MZHigh - Maximum parent ion m/z value to be included in event list
#' @param RTLow - Minimum retention time value to be included in event list
#' @param RTHigh - Maximum retention time value to be included in event list
#' @return Data frame of fragmentation events inside of desired LC-MS region
#' Includes Scan Number (to use with function GetSpectraByScanNumber),
#' Mass of parent ion,
#' Retention time of fragmentation event
#' MSOrder - if 2 - that is MSMS spectra, if more it is high order MSN spectra
#' Desc - description of spectrum
#' @examples
#' GetFragmentationEvents("160215_LCMS_QE_pHILIC_Nina_Complete_Labeling_Neg_57", 200, 210, 11, 12)
#' @export
GetFragmentationEvents =
  function(FileName, MZLow, MZHigh, RTLow, RTHigh){
    iface = get("iface", envir = WDSLEnvir)
    SPInfo = iface@functions$GetFragmentationEvents(list(
      FileName=FileName,
      MZLow = MZLow, MZHigh = MZHigh,
      RTLow = RTLow, RTHigh = RTHigh))
    if (is.character(SPInfo@ErrorMessage)) print(SPInfo@ErrorMessage)
    df = data.frame(
      ScanNumber=as.numeric(sapply(SPInfo@GetFragmentationEventsResult,function(x)x@ScanNumber,simplify = TRUE )),
      ParentMZ=as.numeric(sapply(SPInfo@GetFragmentationEventsResult,function(x)x@ParentMZ,simplify = TRUE )),
      RT=as.numeric(sapply(SPInfo@GetFragmentationEventsResult,function(x)x@RT,simplify = TRUE )),
      MSOrder=as.numeric(sapply(SPInfo@GetFragmentationEventsResult,function(x)x@MSOrder,simplify = TRUE )),
      Description = as.character(sapply(SPInfo@GetFragmentationEventsResult,function(x)x@Description,simplify = TRUE ))
    )
    return(df)
  }




#' Get Array of XIC Chromatogram
#'
#' Batch analog of GetChromatogram function.
#' Gets array of eXtracted Ion Current (XIC) chromatograms for specified LC-MS areas.
#' Equal length of incoming lists is assumed.
#' Corresponds to mzAccess web-service API function GetChromatogramsArray
#'
#' @param FileNames - List of names of original raw mass spectrometry file.
#' @param MZLow - List of minimum m/z values for LC-MS areas requested
#' @param MZHigh - List of maximum m/z values for LC-MS areas requested
#' @param RTLow - List of minimum retention time values for LC-MS areas requested
#' @param RTHigh - List of maximum retention time values for LC-MS areas requested
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @return List of data frames. Each n-th data frame represent single XIC chromatogram specified
#'  by n-th members of incoming arrays and consist of
#'  of Retention Time and Intensities for requested LC-MS area
#' @examples
#' GetFragmentationEvents("160215_LCMS_QE_pHILIC_Nina_Complete_Labeling_Neg_57", 200, 210, 11, 12)
#' @export
GetChromatogramArray =
  function(FileNames,MZLows,MZHighs,RTLows,RTHighs,Cache=TRUE){
    iface = get("iface", envir = WDSLEnvir)
    ArAr=iface@functions$GetChromatogramArray(list(
      FileNames=FileNames,
      MZLow=MZLows,
      MZHigh=MZHighs,
      RTLow=RTLows,
      RTHigh=RTHighs,
      Cache=Cache))
    if (is.character(ArAr@ErrorMessage)) print(ArAr@ErrorMessage)
    Res=lapply(ArAr@GetChromatogramArrayResult,function(L){
      data.frame(
        RT=L[c(TRUE,FALSE)],
        Intensity=L[c(FALSE,TRUE)])})
    return(Res)
  }

#' Get Array of LC-MS Area signals
#'
#' Batch analog of GetArea function.
#' Gets List of data frames where each data frame contains all non-zero signals for one of specified LC-MS areas.
#' Equal length of incoming lists is assumed.
#' Corresponds to mzAccess web-service API function GetAreasArray
#'
#' @param FileNames - List of names of original raw mass spectrometry file.
#' @param MZLow - List of minimum m/z values for LC-MS areas requested
#' @param MZHigh - List of maximum m/z values for LC-MS areas requested
#' @param RTLow - List of minimum retention time values for LC-MS areas requested
#' @param RTHigh - List of maximum retention time values for LC-MS areas requested
#' @param Cache - If TRUE data will be loaded from fast access cache, if FALSE - from original raw files
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @return List of data frames. Each n-th data frame contains Mass, Retention Time and Intensities for all non-zero signals for LC-MS area, specified
#'  by n-th members of incoming arrays.
#' @examples
#' GetFragmentationEvents("160215_LCMS_QE_pHILIC_Nina_Complete_Labeling_Neg_57", 200, 210, 11, 12)
#' @export
GetAreaArray =
  function(FileNames,MZLows,MZHighs,RTLows,RTHighs,Cache=TRUE,Profile=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    ArAr=iface@functions$GetAreaArray(list(
      FileNames=FileNames,
      MZLow=MZLows,
      MZHigh=MZHighs,
      RTLow=RTLows,
      RTHigh=RTHighs,
      Cache=Cache,
      Profile=Profile))
    if (is.character(ArAr@ErrorMessage)) print(ArAr@ErrorMessage)
    Res=lapply(ArAr@GetAreaArrayResult,function(L){
      data.frame(
        Mass=L[c(TRUE,FALSE,FALSE)],
        RT=L[c(FALSE,TRUE,FALSE)],
        Intensity=L[c(FALSE,FALSE,TRUE)])})
    return(Res)
  }


#' Get Array of averaged spectra
#'
#' Batch analog of GetAveSpectrum  function.
#' Get Array of spectra averaged (or summed) within desired RT intervals
#' Corresponds to mzAccess web-service API function GetSpectrumArray
#' Equal length of incoming lists is assumed.
#' Averaging algorithm is  vendor specific, therefore function has no cache option
#'
#' @param FileName - Name of original raw mass spectrometry file. Can be stated with or without path and extention
#' @param MZLow - List of minimum m/z values to be included in spectrum
#' @param MZHigh - List of maximum m/z values to be included in spectrum
#' @param RTLow - List of minimum retention time values for RT range of averaged spectrum
#' @param RTHigh - List of maximum retention time values for RT range of averaged spectrum
#' @param Profile - If TRUE data will presented in profile mode how is was acquired by mass spectrometer, If FALSE data will be presented in peak centroided mode
#' @return List of data frames of Mass and Intensities for requested averaged spectra
#' @examples
#' GetAvgSpectrum("031_MOO_Labeling_HIL_72h_100_3d_MM", MZLow = 50, MZHigh = 400, RTLow = 6, RTHigh = 8, Profile = FALSE)
#' @export
GetSpectrumArray =
  function(FileNames,MZLows,MZHighs,RTLows,RTHighs,Profile=FALSE){
    iface = get("iface", envir = WDSLEnvir)
    ArAr=iface@functions$GetSpectraArray(list(
      FileNames=FileNames,
      MZLow=MZLows,
      MZHigh=MZHighs,
      RTLow=RTLows,
      RTHigh=RTHighs,
      Profile=Profile))
    if (is.character(ArAr@ErrorMessage)) print(ArAr@ErrorMessage)
    Res=lapply(ArAr@GetSpectrumArrayResult,function(L){
      data.frame(
        Mass=L[c(TRUE,FALSE)],
        Intensity=L[c(FALSE,TRUE)])})
    return(Res)
  }


