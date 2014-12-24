(defun random-background-color ()
  ;; Color stuff
  (interactive)
  (if (and (not (eq t x-no-window-manager)) (x-display-color-p))
      (let ((my-background-color-preferences
	     '("gainsboro" "honeydew" "mistyrose" "slategrey" "skyblue"
	       "light steel blue"
	       "lightcyan" "goldenrod" "peru" "light salmon" "medium violet red"
	       "snow4" "bisque2" "bisque3" "azure1" "slateblue1" "skyblue1"
	       "lightskyblue3" "paleturquoise4" "cadetblue2" "aquamarine4"
	       "springgreen2" "olivedrab1" "yellow2" "goldenrod3" "goldenrod4"
	       "burlywood1" "orange3" "darkorchid2" "mediumpurple2" "gray63"
	       "gray99" 
	       "deep sky blue" "dark khaki" "wheat" "dark orange" "coral"
	       "medium purple"
	       "thistle" "navajowhite3" "lemonchiffon1" "honeydew1"
	       "paleturquoise1" "goldenrod2" "burlywood2" "chocolate2" "maroon1"
	       "maroon2" "darkorchid1" "mediumpurple1" "thistle1" "thistle2"
	       "thistle3" "gray74" 
	       "medium spring green"
	       "olive drab" "darkgoldenrod" "rosybrown" "violetred" "slateblue3"
	       "deepskyblue2" "deepskyblue3" "deepskyblue4" "cadetblue4"
	       "wheat4" "lightsalmon2" "darkorange1" "grey61" "gray65" "grey100"
	       "floralwhite" "lemon chiffon" "light slate grey"
	       "dodger blue" "steelblue" "turquoise" "medium aquamarine"
	       "lawngreen" "lightyellow" "dark goldenrod" "violet"
	       "medium orchid" "navajowhite4" "ivory1" "honeydew4"
	       "deepskyblue1"
	       "darkolivegreen3" "lightgoldenrod3" "darkgoldenrod1" "wheat1"
	       "pink2" "orchid4" "grey85" "grey88"
	       "dark gray" "lightgreen"
	       "lightslategrey" "lightskyblue"
	       "rosy brown" "beige"
	       "deeppink" "mediumvioletred" "dark orchid" "peachpuff3"
	       "lightcyan4" "palegreen3" "plum3" "grey77"
	       "floral white"
	       "antique white" "aliceblue" "light gray" "dodgerblue"
	       "mediumaquamarine"
	       "khaki" "lightgoldenrod" "hot pink" "light pink" "antiquewhite4"
	       "steelblue2" "turquoise3" "darkslategray4" "aquamarine1"
	       "darkseagreen2" "darkseagreen3" "seagreen1" "lightyellow4"
	       "rosybrown2" "sienna2" "magenta1" "orchid1" "gray61"
	       "gray85" "grey86" "grey87" "gray94" "grey94" "grey97" "gray100"
	       "bisque" "mint cream"
	       "slate grey" "lightslategray" "powderblue" "light sea green"
	       "greenyellow" "darkkhaki" "seashell2" "peachpuff2"
	       "lemonchiffon2" "lightsteelblue1" "lightsteelblue2" "palegreen2"
	       "springgreen1" "deeppink2" "grey67" "grey71"
	       "whitesmoke" "navajowhite" "alice blue" "lightgrey"
	       "cornflowerblue" "lawn green" "magenta" "snow3" "antiquewhite1"
	       "bisque1" "navajowhite1" "navajowhite2" "steelblue1"
	       "lightskyblue2" "lightblue3" "turquoise2" "khaki4" "lightyellow3"
	       "yellow1" "wheat2" "chocolate1" "orange1" "orange2" "lightpink4"
	       "magenta3" "magenta4" "purple2" "purple3"
	       "gray80" "grey80" "grey95" "grey96" "dark magenta"
	       "lemonchiffon"
	       "lavender blush" "slate blue" "light cyan" "cadet blue"
	       "mediumseagreen"
	       "green yellow" "tan" "darksalmon" "lightsalmon" "darkorange"
	       "light coral" "deep pink" "pale violet red" "darkviolet" "snow2"
	       "lightskyblue1" "khaki2" "lightgoldenrod4" "lightsalmon1" "pink3"
	       "magenta2" "mediumorchid2" "grey68" "grey69"
	       "grey73" "grey74" "grey75" "gray81" "gray90"
	       "old lace" "peach puff" "grey" "lightgray"
	       "light slate blue" "powder blue" "lightgoldenrodyellow" "gold"
	       "lightpink" "orchid" "bisque4" "cornsilk1" "honeydew2"
	       "lightblue4" "olivedrab4" "khaki3" "sienna1" "palevioletred3"
	       "darkorchid3" "darkorchid4" "mediumpurple3"
	       "grey72" "gray75" "grey82" "grey83" "grey84"
	       "blanched almond"
	       "peachpuff" "moccasin" "mintcream" "lavenderblush"
	       "light slate gray" "gray" "cornflower blue" "palegreen"
	       "chartreuse" "salmon"
	       "lightcoral" "pink" "violet red" "lavenderblush1"
	       "lavenderblush2" "lavenderblush3" "mistyrose1" "mistyrose4"
	       "azure4" "skyblue2" "lightcyan3" "aquamarine2" "olivedrab2"
	       "darkolivegreen1" "tan3" "tomato1" "maroon3" "orchid3" "grey62"
	       "gray66" "grey81" "gray83" "gray92"
	       "indigo2" "antiquewhite" "sky blue"
	       "light sky blue" "steel blue" "darkseagreen" "yellow green"
	       "palegoldenrod" "lightsteelblue4" "darkslategray1"
	       "darkslategray2" "darkslategray3" "olivedrab3" "darkolivegreen4"
	       "coral1" "lightpink2" "palevioletred4" "orchid2" "plum1"
	       "thistle4" "gray77" "gray78" "gray79"
	       "medium turquoise" "cyan" "cadetblue" "springgreen" "hotpink"
	       "mediumorchid" "antiquewhite3" "lemonchiffon4" "mistyrose2"
	       "steelblue3" "skyblue3" "lightcyan2" "darkseagreen1" "yellow3"
	       "gold1" "gold3" "tan2" "salmon1" "plum4" "grey90"
	       "ivory" "dark salmon" "dark violet" "lightblue1"
	       "lightblue2" "turquoise1" "lightyellow2" "yellow4" "coral3"
	       "deeppink3" "hotpink1" "lightpink1" "mediumorchid4"
	       "grey63" "gray67" "gray73" "grey89" "dark cyan"
	       "darkcyan"
	       "ghostwhite" "slategray" "lightslateblue" "pale green"
	       "seashell3" "peachpuff4" "cornsilk2" "cornsilk4" "ivory3"
	       "honeydew3" "azure2" "slategray2" "slategray3" "slategray4"
	       "cadetblue3" "darkseagreen4" "springgreen3" "khaki1"
	       "lightgoldenrod2" "goldenrod1" "mediumorchid3" "purple1"
	       "gray60" "grey60" "gray64" "grey70" "gray71" "gray89" "gray98"
	       "navajo white" "white"
	       "paleturquoise2" "cadetblue1" "cyan3" "aquamarine3"
	       "lightyellow1" "burlywood3" "wheat3" "lightpink3"
	       "palevioletred1" "palevioletred2" "gray62" "gray72"
	       "deepskyblue" "snow1" "azure3" "slateblue2" "darkgoldenrod2"
	       "darkgoldenrod3" "tan1" "violetred1" "violetred2" "violetred3"
	       "grey76" "grey79" "gray84" "gray93"
	       "oldlace" "linen" "lavender"
	       "misty rose" "dark turquoise" "darkturquoise"
	       "light goldenrod yellow" "lavenderblush4" "seagreen2"
	       "rosybrown4" "lightsalmon3"
	       "deeppink1" "gray70" "grey78" "gray86" "gray87"
	       "gray88" "gray95" "gray96" "gray97" "snow"
	       "ghost white" "white smoke" "mediumslateblue" "spring green"
	       "light yellow" "light goldenrod" "blueviolet" "seashell4"
	       "peachpuff1"
	       "lightskyblue4" "paleturquoise3" "cyan1" "cyan2" "palegreen1"
	       "lightgoldenrod1" "rosybrown3" "sienna3" "gray82" "gray91"
	       "darkgray"
	       "blanchedalmond" "cornsilk" "lightseagreen"
	       "plum" "lemonchiffon3" "ivory2" "cyan4" "rosybrown1" "salmon2"
	       "darkorange2" "pink1" "grey91" "grey92" "grey93"
	       "papaya whip"
	       "papayawhip" "azure" "lightsteelblue" "aquamarine"
	       "mediumspringgreen" "pale goldenrod" "sandy brown" "mistyrose3"
	       "lightcyan1" "darkolivegreen2" "hotpink2" "hotpink3" "grey64"
	       "grey65" "grey66" "gray68" "gray69" "gray76" "darkgrey"
	       "seashell" "light grey"
	       "medium slate blue" "lightblue" "pale turquoise" "paleturquoise"
	       "yellow" "burlywood" "sandybrown" "orange" "purple"
	       "antiquewhite2" "ivory4" "seagreen3" "chartreuse1" "gold2"
	       "coral2" "pink4" "plum2" "grey99" "dark grey" "light green"
	       "slate gray" "light blue" "mediumturquoise"
	       "dark sea green" "palevioletred" "darkorchid" "blue violet"
	       "mediumpurple" "seashell1" "cornsilk3" "dodgerblue1"
	       "dodgerblue2" "slategray1" "lightsteelblue3" "orangered4"
	       "mediumorchid1" "grey98" "darkmagenta"))
	    randy)
	(while (progn (setq randy (nth 
				   (mod (random t)
					(length
					 my-background-color-preferences))
				   my-background-color-preferences))
		      (not (x-color-defined-p randy)))
	  (message "%s unavailable as background color" randy))
	(set-background-color randy)
	(message "Background set to %s" randy)
	randy)))

(random-background-color)
