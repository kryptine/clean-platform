implementation module Text.Encodings.MIME

import StdOverloaded, StdString, StdList, StdArray
import Data.Maybe, Text

encodeMimeMultipart :: !MIMEBoundary ![MIMEPart] -> String
encodeMimeMultipart boundary parts
	= "This is a message with multiple parts in MIME format.\r\n" 
	+++ "--" +++ boundary +++ "\r\n"
	+++ join ("\r\n--" +++ boundary +++ "\r\n") [formatPart headers body  \\ (headers,body) <- parts]
	+++ "\r\n--" +++ boundary +++ "--"
where
	formatPart [] body		= body
	formatPart headers body = join "\r\n" [name +++ ": " +++ value \\ (name,value) <- headers ] +++ "\r\n\r\n" +++ body

decodeMimeMultipart :: !MIMEBoundary !String -> [MIMEPart]
decodeMimeMultipart boundary body
	# startindex		= indexOf ("--" +++ boundary +++ "\r\n") body //Locate the first boundary
	| startindex == -1	= [] //Fail
	# endindex			= indexOf ("\r\n" +++ "--" +++ boundary +++ "--") body //Locate the final boundary
	| endindex == -1	= [] //Fail
	# body				= body % (startindex + (size boundary) + 4, endindex - 1)
	# parts				= split ("\r\n" +++ "--" +++ boundary +++ "\r\n") body
	= map parsePart parts
where
	parsePart :: String -> ([(!String,!String)], String)
	parsePart part 
		# index 		= indexOf "\r\n\r\n" part
		| index < 1 	= ([], part)
						= ([fromJust header \\ header <- map parseHeader (split "\r\n" (part % (0, index - 1))) | isJust header]
							, part % (index + 4, size part))

	parseHeader :: !String -> Maybe (!String, !String)
	parseHeader header
		# index                 = indexOf ":" header
		| index < 1             = Nothing
		# name                  = trim (header % (0, index - 1))
		# value                 = trim (header % (index + 1, size header))
		= Just (name,value)

extensionToMimeType :: !String -> MIMEType
extensionToMimeType "3dm" = "x-world/x-3dmf"
extensionToMimeType "3dmf" = "x-world/x-3dmf"
extensionToMimeType "a" = "application/octet-stream"
extensionToMimeType "aab" = "application/x-authorware-bin"
extensionToMimeType "aam" = "application/x-authorware-map"
extensionToMimeType "aas" = "application/x-authorware-seg"
extensionToMimeType "abc" = "text/vnd.abc"
extensionToMimeType "acgi" = "text/html"
extensionToMimeType "afl" = "video/animaflex"
extensionToMimeType "ai" = "application/postscript"
extensionToMimeType "aif" = "audio/aiff"
extensionToMimeType "aifc" = "audio/aiff"
extensionToMimeType "aiff" = "audio/aiff"
extensionToMimeType "aim" = "application/x-aim"
extensionToMimeType "aip" = "text/x-audiosoft-intra"
extensionToMimeType "ani" = "application/x-navi-animation"
extensionToMimeType "aos" = "application/x-nokia-9000-communicator-add-on-software"
extensionToMimeType "aps" = "application/mime"
extensionToMimeType "arc" = "application/octet-stream"
extensionToMimeType "arj" = "application/arj"
extensionToMimeType "art" = "image/x-jg"
extensionToMimeType "asf" = "video/x-ms-asf"
extensionToMimeType "asm" = "text/x-asm"
extensionToMimeType "asp" = "text/asp"
extensionToMimeType "asx" = "video/x-ms-asf-plugin"
extensionToMimeType "au" = "audio/x-au"
extensionToMimeType "avi" = "video/avi"
extensionToMimeType "avs" = "video/avs-video"
extensionToMimeType "bcpio" = "application/x-bcpio"
extensionToMimeType "bin" = "application/x-macbinary"
extensionToMimeType "bm" = "image/bmp"
extensionToMimeType "bmp" = "image/x-windows-bmp"
extensionToMimeType "boo" = "application/book"
extensionToMimeType "book" = "application/book"
extensionToMimeType "boz" = "application/x-bzip2"
extensionToMimeType "bsh" = "application/x-bsh"
extensionToMimeType "bz" = "application/x-bzip"
extensionToMimeType "bz2" = "application/x-bzip2"
extensionToMimeType "c" = "text/plain"
extensionToMimeType "c++" = "text/plain"
extensionToMimeType "cat" = "application/vnd.ms-pki.seccat"
extensionToMimeType "cc" = "text/plain"
extensionToMimeType "ccad" = "application/clariscad"
extensionToMimeType "cco" = "application/x-cocoa"
extensionToMimeType "cdf" = "application/cdf"
extensionToMimeType "cer" = "application/x-x509-ca-cert"
extensionToMimeType "cha" = "application/x-chat"
extensionToMimeType "chat" = "application/x-chat"
extensionToMimeType "class" = "application/java"
extensionToMimeType "com" = "application/octet-stream"
extensionToMimeType "conf" = "text/plain"
extensionToMimeType "cpio" = "application/x-cpio"
extensionToMimeType "cpp" = "text/x-c"
extensionToMimeType "cpt" = "application/x-cpt"
extensionToMimeType "crl" = "application/pkcs-crl"
extensionToMimeType "crt" = "application/x-x509-ca-cert"
extensionToMimeType "csh" = "text/x-script.csh"
extensionToMimeType "css" = "text/css"
extensionToMimeType "cxx" = "text/plain"
extensionToMimeType "dcr" = "application/x-director"
extensionToMimeType "deepv" = "application/x-deepv"
extensionToMimeType "def" = "text/plain"
extensionToMimeType "der" = "application/x-x509-ca-cert"
extensionToMimeType "dif" = "video/x-dv"
extensionToMimeType "dir" = "application/x-director"
extensionToMimeType "dl" = "video/dl"
extensionToMimeType "doc" = "application/msword"
extensionToMimeType "dot" = "application/msword"
extensionToMimeType "dp" = "application/commonground"
extensionToMimeType "drw" = "application/drafting"
extensionToMimeType "dump" = "application/octet-stream"
extensionToMimeType "dv" = "video/x-dv"
extensionToMimeType "dvi" = "application/x-dvi"
extensionToMimeType "dwf" = "model/vnd.dwf"
extensionToMimeType "dwg" = "image/x-dwg"
extensionToMimeType "dxf" = "image/x-dwg"
extensionToMimeType "dxr" = "application/x-director"
extensionToMimeType "el" = "text/x-script.elisp"
extensionToMimeType "elc" = "application/x-elc"
extensionToMimeType "env" = "application/x-envoy"
extensionToMimeType "eps" = "application/postscript"
extensionToMimeType "es" = "application/x-esrehber"
extensionToMimeType "etx" = "text/x-setext"
extensionToMimeType "evy" = "application/envoy"
extensionToMimeType "exe" = "application/octet-stream"
extensionToMimeType "f" = "text/plain"
extensionToMimeType "f77" = "text/x-fortran"
extensionToMimeType "f90" = "text/plain"
extensionToMimeType "fdf" = "application/vnd.fdf"
extensionToMimeType "fif" = "image/fif"
extensionToMimeType "fli" = "video/fli"
extensionToMimeType "flo" = "image/florian"
extensionToMimeType "flv" = "video/flv"
extensionToMimeType "flx" = "text/vnd.fmi.flexstor"
extensionToMimeType "fmf" = "video/x-atomic3d-feature"
extensionToMimeType "for" = "text/plain"
extensionToMimeType "fpx" = "image/vnd.fpx"
extensionToMimeType "frl" = "application/freeloader"
extensionToMimeType "funk" = "audio/make"
extensionToMimeType "g" = "text/plain"
extensionToMimeType "g3" = "image/g3fax"
extensionToMimeType "gif" = "image/gif"
extensionToMimeType "gl" = "video/gl"
extensionToMimeType "gsd" = "audio/x-gsm"
extensionToMimeType "gsm" = "audio/x-gsm"
extensionToMimeType "gsp" = "application/x-gsp"
extensionToMimeType "gss" = "application/x-gss"
extensionToMimeType "gtar" = "application/x-gtar"
extensionToMimeType "gz" = "application/x-gzip"
extensionToMimeType "gzip" = "application/x-gzip"
extensionToMimeType "h" = "text/plain"
extensionToMimeType "hdf" = "application/x-hdf"
extensionToMimeType "help" = "application/x-helpfile"
extensionToMimeType "hgl" = "application/vnd.hp-hpgl"
extensionToMimeType "hh" = "text/plain"
extensionToMimeType "hlb" = "text/x-script"
extensionToMimeType "hlp" = "application/hlp"
extensionToMimeType "hpg" = "application/vnd.hp-hpgl"
extensionToMimeType "hpgl" = "application/vnd.hp-hpgl"
extensionToMimeType "hqx" = "application/binhex"
extensionToMimeType "hta" = "application/hta"
extensionToMimeType "htc" = "text/x-component"
extensionToMimeType "htm" = "text/html"
extensionToMimeType "html" = "text/html"
extensionToMimeType "htmls" = "text/html"
extensionToMimeType "htt" = "text/webviewhtml"
extensionToMimeType "htx" = "text/html"
extensionToMimeType "ice" = "x-conference/x-cooltalk"
extensionToMimeType "ico" = "image/x-icon"
extensionToMimeType "idc" = "text/plain"
extensionToMimeType "ief" = "image/ief"
extensionToMimeType "iefs" = "image/ief"
extensionToMimeType "iges" = "application/iges"
extensionToMimeType "igs" = "application/iges"
extensionToMimeType "ima" = "application/x-ima"
extensionToMimeType "imap" = "application/x-httpd-imap"
extensionToMimeType "inf" = "application/inf"
extensionToMimeType "ins" = "application/x-internett-signup"
extensionToMimeType "ip" = "application/x-ip2"
extensionToMimeType "isu" = "video/x-isvideo"
extensionToMimeType "it" = "audio/it"
extensionToMimeType "iv" = "application/x-inventor"
extensionToMimeType "ivr" = "i-world/i-vrml"
extensionToMimeType "ivy" = "application/x-livescreen"
extensionToMimeType "jam" = "audio/x-jam"
extensionToMimeType "jav" = "text/plain"
extensionToMimeType "java" = "text/plain"
extensionToMimeType "jcm" = "application/x-java-commerce"
extensionToMimeType "jfif" = "image/jpeg"
extensionToMimeType "jfif-tbnl" = "image/jpeg"
extensionToMimeType "jpe" = "image/jpeg"
extensionToMimeType "jpeg" = "image/jpeg"
extensionToMimeType "jpg" = "image/jpeg"
extensionToMimeType "jps" = "image/x-jps"
extensionToMimeType "js" = "application/javascript"
extensionToMimeType "jut" = "image/jutvision"
extensionToMimeType "kar" = "audio/midi"
extensionToMimeType "ksh" = "application/x-ksh"
extensionToMimeType "la" = "audio/nspaudio"
extensionToMimeType "lam" = "audio/x-liveaudio"
extensionToMimeType "latex" = "application/x-latex"
extensionToMimeType "lha" = "application/lha"
extensionToMimeType "lhx" = "application/octet-stream"
extensionToMimeType "list" = "text/plain"
extensionToMimeType "lma" = "audio/nspaudio"
extensionToMimeType "log" = "text/plain"
extensionToMimeType "lsp" = "application/x-lisp"
extensionToMimeType "lst" = "text/plain"
extensionToMimeType "ltx" = "application/x-latex"
extensionToMimeType "lzh" = "application/octet-stream"
extensionToMimeType "lzx" = "application/lzx"
extensionToMimeType "m" = "text/plain"
extensionToMimeType "m1v" = "video/mpeg"
extensionToMimeType "m2a" = "audio/mpeg"
extensionToMimeType "m2v" = "video/mpeg"
extensionToMimeType "m3u" = "audio/x-mpequrl"
extensionToMimeType "man" = "application/x-troff-man"
extensionToMimeType "map" = "application/x-navimap"
extensionToMimeType "mar" = "text/plain"
extensionToMimeType "mbd" = "application/mbedlet"
extensionToMimeType "mc$" = "application/x-magic-cap-package-1.0"
extensionToMimeType "mcd" = "application/mcad"
extensionToMimeType "mcf" = "image/vasa"
extensionToMimeType "mcp" = "application/netmc"
extensionToMimeType "me" = "application/x-troff-me"
extensionToMimeType "mht" = "message/rfc822"
extensionToMimeType "mhtml" = "message/rfc822"
extensionToMimeType "mid" = "audio/midi"
extensionToMimeType "midi" = "audio/midi"
extensionToMimeType "mif" = "application/x-mif"
extensionToMimeType "mime" = "www/mime"
extensionToMimeType "mjf" = "audio/x-vnd.audioexplosion.mjuicemediafile"
extensionToMimeType "mjpg" = "video/x-motion-jpeg"
extensionToMimeType "mm" = "application/base64"
extensionToMimeType "mme" = "application/base64"
extensionToMimeType "mod" = "audio/mod"
extensionToMimeType "moov" = "video/quicktime"
extensionToMimeType "mov" = "video/quicktime"
extensionToMimeType "movie" = "video/x-sgi-movie"
extensionToMimeType "mp2" = "audio/mpeg"
extensionToMimeType "mp3" = "audio/mpeg3"
extensionToMimeType "mpa" = "audio/mpeg"
extensionToMimeType "mpc" = "application/x-project"
extensionToMimeType "mpe" = "video/mpeg"
extensionToMimeType "mpeg" = "video/mpeg"
extensionToMimeType "mpg" = "video/mpeg"
extensionToMimeType "mpga" = "audio/mpeg"
extensionToMimeType "mpp" = "application/vnd.ms-project"
extensionToMimeType "mpt" = "application/x-project"
extensionToMimeType "mpv" = "application/x-project"
extensionToMimeType "mpx" = "application/x-project"
extensionToMimeType "mrc" = "application/marc"
extensionToMimeType "ms" = "application/x-troff-ms"
extensionToMimeType "mv" = "video/x-sgi-movie"
extensionToMimeType "my" = "audio/make"
extensionToMimeType "mzz" = "application/x-vnd.audioexplosion.mzz"
extensionToMimeType "nap" = "image/naplps"
extensionToMimeType "naplps" = "image/naplps"
extensionToMimeType "nc" = "application/x-netcdf"
extensionToMimeType "ncm" = "application/vnd.nokia.configuration-message"
extensionToMimeType "nif" = "image/x-niff"
extensionToMimeType "niff" = "image/x-niff"
extensionToMimeType "nix" = "application/x-mix-transfer"
extensionToMimeType "nsc" = "application/x-conference"
extensionToMimeType "nvd" = "application/x-navidoc"
extensionToMimeType "o" = "application/octet-stream"
extensionToMimeType "oda" = "application/oda"
extensionToMimeType "omc" = "application/x-omc"
extensionToMimeType "omcd" = "application/x-omcdatamaker"
extensionToMimeType "omcr" = "application/x-omcregerator"
extensionToMimeType "p" = "text/x-pascal"
extensionToMimeType "p10" = "application/pkcs10"
extensionToMimeType "p12" = "application/pkcs-12"
extensionToMimeType "p7a" = "application/x-pkcs7-signature"
extensionToMimeType "p7c" = "application/pkcs7-mime"
extensionToMimeType "p7m" = "application/pkcs7-mime"
extensionToMimeType "p7r" = "application/x-pkcs7-certreqresp"
extensionToMimeType "p7s" = "application/pkcs7-signature"
extensionToMimeType "part" = "application/pro_eng"
extensionToMimeType "pas" = "text/pascal"
extensionToMimeType "pbm" = "image/x-portable-bitmap"
extensionToMimeType "pcl" = "application/vnd.hp-pcl"
extensionToMimeType "pct" = "image/x-pict"
extensionToMimeType "pcx" = "image/x-pcx"
extensionToMimeType "pdb" = "chemical/x-pdb"
extensionToMimeType "pdf" = "application/pdf"
extensionToMimeType "pfunk" = "audio/make.my.funk"
extensionToMimeType "pgm" = "image/x-portable-graymap"
extensionToMimeType "pic" = "image/pict"
extensionToMimeType "pict" = "image/pict"
extensionToMimeType "pkg" = "application/x-newton-compatible-pkg"
extensionToMimeType "pko" = "application/vnd.ms-pki.pko"
extensionToMimeType "pl" = "text/plain"
extensionToMimeType "plx" = "application/x-pixclscript"
extensionToMimeType "pm" = "text/x-script.perl-module"
extensionToMimeType "pm4" = "application/x-pagemaker"
extensionToMimeType "pm5" = "application/x-pagemaker"
extensionToMimeType "png" = "image/png"
extensionToMimeType "pnm" = "image/x-portable-anymap"
extensionToMimeType "pot" = "application/mspowerpoint"
extensionToMimeType "pov" = "model/x-pov"
extensionToMimeType "ppa" = "application/vnd.ms-powerpoint"
extensionToMimeType "ppm" = "image/x-portable-pixmap"
extensionToMimeType "pps" = "application/mspowerpoint"
extensionToMimeType "ppt" = "application/mspowerpoint"
extensionToMimeType "ppz" = "application/mspowerpoint"
extensionToMimeType "pre" = "application/x-freelance"
extensionToMimeType "prt" = "application/pro_eng"
extensionToMimeType "ps" = "application/postscript"
extensionToMimeType "psd" = "application/octet-stream"
extensionToMimeType "pvu" = "paleovu/x-pv"
extensionToMimeType "pwz" = "application/vnd.ms-powerpoint"
extensionToMimeType "py" = "text/x-script.phyton"
extensionToMimeType "pyc" = "applicaiton/x-bytecode.python"
extensionToMimeType "qcp" = "audio/vnd.qcelp"
extensionToMimeType "qd3" = "x-world/x-3dmf"
extensionToMimeType "qd3d" = "x-world/x-3dmf"
extensionToMimeType "qif" = "image/x-quicktime"
extensionToMimeType "qt" = "video/quicktime"
extensionToMimeType "qtc" = "video/x-qtc"
extensionToMimeType "qti" = "image/x-quicktime"
extensionToMimeType "qtif" = "image/x-quicktime"
extensionToMimeType "ra" = "audio/realaudio"
extensionToMimeType "ram" = "audio/x-pn-realaudio"
extensionToMimeType "ras" = "application/x-cmu-raster"
extensionToMimeType "rast" = "image/cmu-raster"
extensionToMimeType "rexx" = "text/x-script.rexx"
extensionToMimeType "rf" = "image/vnd.rn-realflash"
extensionToMimeType "rgb" = "image/x-rgb"
extensionToMimeType "rm" = "audio/x-pn-realaudio"
extensionToMimeType "rmi" = "audio/mid"
extensionToMimeType "rmm" = "audio/x-pn-realaudio"
extensionToMimeType "rmp" = "audio/x-pn-realaudio-plugin"
extensionToMimeType "rng" = "application/ringing-tones"
extensionToMimeType "rnx" = "application/vnd.rn-realplayer"
extensionToMimeType "roff" = "application/x-troff"
extensionToMimeType "rp" = "image/vnd.rn-realpix"
extensionToMimeType "rpm" = "audio/x-pn-realaudio-plugin"
extensionToMimeType "rt" = "text/richtext"
extensionToMimeType "rtf" = "text/richtext"
extensionToMimeType "rtx" = "text/richtext"
extensionToMimeType "rv" = "video/vnd.rn-realvideo"
extensionToMimeType "s" = "text/x-asm"
extensionToMimeType "s3m" = "audio/s3m"
extensionToMimeType "saveme" = "application/octet-stream"
extensionToMimeType "sbk" = "application/x-tbook"
extensionToMimeType "scm" = "video/x-scm"
extensionToMimeType "sdml" = "text/plain"
extensionToMimeType "sdp" = "application/sdp"
extensionToMimeType "sdr" = "application/sounder"
extensionToMimeType "sea" = "application/sea"
extensionToMimeType "set" = "application/set"
extensionToMimeType "sgm" = "text/sgml"
extensionToMimeType "sgml" = "text/sgml"
extensionToMimeType "sh" = "application/x-sh"
extensionToMimeType "shar" = "application/x-shar"
extensionToMimeType "shtml" = "text/html"
extensionToMimeType "sid" = "audio/x-psid"
extensionToMimeType "sit" = "application/x-stuffit"
extensionToMimeType "skd" = "application/x-koan"
extensionToMimeType "skm" = "application/x-koan"
extensionToMimeType "skp" = "application/x-koan"
extensionToMimeType "skt" = "application/x-koan"
extensionToMimeType "sl" = "application/x-seelogo"
extensionToMimeType "smi" = "application/smil"
extensionToMimeType "smil" = "application/smil"
extensionToMimeType "snd" = "audio/basic"
extensionToMimeType "sol" = "application/solids"
extensionToMimeType "spc" = "text/x-speech"
extensionToMimeType "spl" = "application/futuresplash"
extensionToMimeType "spr" = "application/x-sprite"
extensionToMimeType "sprite" = "application/x-sprite"
extensionToMimeType "src" = "application/x-wais-source"
extensionToMimeType "ssi" = "text/x-server-parsed-html"
extensionToMimeType "ssm" = "application/streamingmedia"
extensionToMimeType "sst" = "application/vnd.ms-pki.certstore"
extensionToMimeType "step" = "application/step"
extensionToMimeType "stl" = "application/sla"
extensionToMimeType "stp" = "application/step"
extensionToMimeType "svg" = "image/svg+xml"
extensionToMimeType "sv4cpio" = "application/x-sv4cpio"
extensionToMimeType "sv4crc" = "application/x-sv4crc"
extensionToMimeType "svf" = "image/x-dwg"
extensionToMimeType "svr" = "application/x-world"
extensionToMimeType "swf" = "application/x-shockwave-flash"
extensionToMimeType "t" = "application/x-troff"
extensionToMimeType "talk" = "text/x-speech"
extensionToMimeType "tar" = "application/x-tar"
extensionToMimeType "tbk" = "application/toolbook"
extensionToMimeType "tcl" = "text/x-script.tcl"
extensionToMimeType "tcsh" = "text/x-script.tcsh"
extensionToMimeType "tex" = "application/x-tex"
extensionToMimeType "texi" = "application/x-texinfo"
extensionToMimeType "texinfo" = "application/x-texinfo"
extensionToMimeType "text" = "text/plain"
extensionToMimeType "tgz" = "application/x-compressed"
extensionToMimeType "tif" = "image/tiff"
extensionToMimeType "tiff" = "image/tiff"
extensionToMimeType "tr" = "application/x-troff"
extensionToMimeType "tsi" = "audio/tsp-audio"
extensionToMimeType "tsp" = "audio/tsplayer"
extensionToMimeType "tsv" = "text/tab-separated-values"
extensionToMimeType "turbot" = "image/florian"
extensionToMimeType "txt" = "text/plain"
extensionToMimeType "uil" = "text/x-uil"
extensionToMimeType "uni" = "text/uri-list"
extensionToMimeType "unis" = "text/uri-list"
extensionToMimeType "unv" = "application/i-deas"
extensionToMimeType "uri" = "text/uri-list"
extensionToMimeType "uris" = "text/uri-list"
extensionToMimeType "ustar" = "application/x-ustar"
extensionToMimeType "uu" = "application/octet-stream"
extensionToMimeType "uue" = "text/x-uuencode"
extensionToMimeType "vcd" = "application/x-cdlink"
extensionToMimeType "vcs" = "text/x-vcalendar"
extensionToMimeType "vda" = "application/vda"
extensionToMimeType "vdo" = "video/vdo"
extensionToMimeType "vew" = "application/groupwise"
extensionToMimeType "viv" = "video/vivo"
extensionToMimeType "vivo" = "video/vivo"
extensionToMimeType "vmd" = "application/vocaltec-media-desc"
extensionToMimeType "vmf" = "application/vocaltec-media-file"
extensionToMimeType "voc" = "audio/voc"
extensionToMimeType "vos" = "video/vosaic"
extensionToMimeType "vox" = "audio/voxware"
extensionToMimeType "vqe" = "audio/x-twinvq-plugin"
extensionToMimeType "vqf" = "audio/x-twinvq"
extensionToMimeType "vql" = "audio/x-twinvq-plugin"
extensionToMimeType "vrml" = "application/x-vrml"
extensionToMimeType "vrt" = "x-world/x-vrt"
extensionToMimeType "vsd" = "application/x-visio"
extensionToMimeType "vst" = "application/x-visio"
extensionToMimeType "vsw" = "application/x-visio"
extensionToMimeType "w60" = "application/wordperfect6.0"
extensionToMimeType "w61" = "application/wordperfect6.1"
extensionToMimeType "w6w" = "application/msword"
extensionToMimeType "wav" = "audio/wav"
extensionToMimeType "wb1" = "application/x-qpro"
extensionToMimeType "wbmp" = "image/vnd.wap.wbmp"
extensionToMimeType "web" = "application/vnd.xara"
extensionToMimeType "wiz" = "application/msword"
extensionToMimeType "wk1" = "application/x-123"
extensionToMimeType "wmf" = "windows/metafile"
extensionToMimeType "wml" = "text/vnd.wap.wml"
extensionToMimeType "wmlc" = "application/vnd.wap.wmlc"
extensionToMimeType "wmls" = "text/vnd.wap.wmlscript"
extensionToMimeType "wmlsc" = "application/vnd.wap.wmlscriptc"
extensionToMimeType "word" = "application/msword"
extensionToMimeType "wp" = "application/wordperfect"
extensionToMimeType "wp5" = "application/wordperfect"
extensionToMimeType "wp6" = "application/wordperfect"
extensionToMimeType "wpd" = "application/wordperfect"
extensionToMimeType "wq1" = "application/x-lotus"
extensionToMimeType "wri" = "application/mswrite"
extensionToMimeType "wrl" = "application/x-world"
extensionToMimeType "wrz" = "model/vrml"
extensionToMimeType "wsc" = "text/scriplet"
extensionToMimeType "wsrc" = "application/x-wais-source"
extensionToMimeType "wtk" = "application/x-wintalk"
extensionToMimeType "xbm" = "image/xbm"
extensionToMimeType "xdr" = "video/x-amt-demorun"
extensionToMimeType "xgz" = "xgl/drawing"
extensionToMimeType "xif" = "image/vnd.xiff"
extensionToMimeType "xl" = "application/excel"
extensionToMimeType "xla" = "application/excel"
extensionToMimeType "xlb" = "application/excel"
extensionToMimeType "xlc" = "application/excel"
extensionToMimeType "xld" = "application/excel"
extensionToMimeType "xlk" = "application/excel"
extensionToMimeType "xll" = "application/excel"
extensionToMimeType "xlm" = "application/excel"
extensionToMimeType "xls" = "application/excel"
extensionToMimeType "xlt" = "application/excel"
extensionToMimeType "xlv" = "application/excel"
extensionToMimeType "xlw" = "application/excel"
extensionToMimeType "xm" = "audio/xm"
extensionToMimeType "xml" = "text/xml"
extensionToMimeType "xmz" = "xgl/movie"
extensionToMimeType "xpix" = "application/x-vnd.ls-xpix"
extensionToMimeType "xpm" = "image/xpm"
extensionToMimeType "x-png" = "image/png"
extensionToMimeType "xsr" = "video/x-amt-showrun"
extensionToMimeType "xwd" = "image/x-xwd"
extensionToMimeType "xyz" = "chemical/x-pdb"
extensionToMimeType "z" = "application/x-compressed"
extensionToMimeType "zip" = "application/x-zip-compressed"
extensionToMimeType "zoo" = "application/octet-stream"
extensionToMimeType "zsh" = "text/x-script.zsh"
extensionToMimeType _     = "application/unknown"
