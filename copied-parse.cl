  
 (:fpdef--=--test--*)
 ;; old:
 ;; (fpdef--=--test-- (fpdef =--test? |,|) ($1))
 ;; new:


 
 (:|,--**identifier?|)
 (|,--**identifier| (|,| identifier))
 
 (=--test (= test) ($1))
 (:|,--fpdef--[=--test]*|)
 (|,--fpdef--[=--test]| (|,| fpdef =--test?))
 (fpdef :or
	((identifier) . ($1))
	((|(| fplist |)|) . ($2)))
 (fplist (fpdef comma--fpdef* comma?) ((cons $1 $2)))
 (comma--fpdef (|,| fpdef) ($2))
 (:comma--fpdef*)


 ;; old:
 #+(or)(varargslist :or
	      ((fpdef--=--test--* * identifier |,--**identifier?|) . ((list $1 $3 $4)))
	      ((fpdef--=--test--* ** identifier) . ((list $1 nil $3)))
	      ((fpdef =--test? |,--fpdef--[=--test]*| comma) . ((list $1 $2)))
 	      ((fpdef =--test? |,--fpdef--[=--test]*|) . ((list $1 $2))))
 ;; next try:
 #+(or)(varargslist :or
		    ((fpdef--=--test--* * identifier |,--**identifier?|) . ((list $1 $3 $4)))
		    ((fpdef--=--test--* ** identifier) . ((list $1 nil $3)))
		    ((fpdef |,--fpdef--[=--test]*| comma) . ((list $1 $2)))
		    ((fpdef = test |,--fpdef--[=--test]*| comma) . ((list $1 $2)))
		    ((fpdef |,--fpdef--[=--test]*|) . ((list $1 $2)))
		    ((fpdef = test |,--fpdef--[=--test]*|) . ((list $1 $2))))
