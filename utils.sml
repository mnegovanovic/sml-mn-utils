structure MNUtils :> sig
    val println: string -> unit
    val reverse: 'a list -> 'a list
    val replaceAll: string -> string -> string -> string
    val prefixLength: int * int -> string
    val readFile: string -> TextIO.vector (* TextIO.vector = string *)
    val writeFile: string -> TextIO.vector -> unit
    val appendToLogFile: string -> TextIO.vector -> unit
    val strip: string -> char list -> string
    val capitalize: string -> string
    val recordName: string -> string
    val isSubset: (''a list) -> (''a list) -> bool
    val hasDuplicates: (''a list) -> bool
    val urlencode: string -> string
    val urldecode: string -> string
    val parseHttpQueryString: string -> (string*string) list
    val test: string -> ''a -> (''a -> string) -> (unit -> ''a) -> unit
end =

struct 
    fun println (s: string): unit =
        print (s ^ "\n")

    fun reverse' [] acc = acc
        | reverse' (x::xs) acc = reverse' xs (x::acc)

    fun reverse l = reverse' l []

    fun replaceAll s ss r =
        let
            val ss_len = String.size ss
            
            fun checkAndReplace acc s =
                if (String.size s) < ss_len then
                    acc^s
                else
                    let
                        val c  = String.substring (s, 0, ss_len)
                    in
                        if c = ss then
                            checkAndReplace (acc^r) (String.extract (s, ss_len, NONE))
                        else
                            checkAndReplace (acc^String.extract (s, 0, SOME 1)) (String.extract (s, 1, NONE))
                    end
        in
            if ss_len > (String.size s) then
                s
            else
                checkAndReplace "" s
        end

    fun prefixLength (i: int, l: int): string =
        if (i < 0) then
            raise Fail "Integer is out of range"
        else
            let
                val i_str = Int.toString i
                val i_len = String.size i_str
                
                fun expand acc 0 = acc
                    | expand acc n = expand ("0"^acc) (n-1)
            in
                expand i_str (l - i_len)
            end

    fun readFile filename =
        let
            val fd = TextIO.openIn filename
            val content = TextIO.inputAll fd handle e => (TextIO.closeIn fd; raise e)
            val _ = TextIO.closeIn fd
        in
            content
        end

    fun writeFile filename content =
        let
            val fd = TextIO.openOut filename
            val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
            val _ = TextIO.closeOut fd
        in
            ()
        end

    val log_files_ : (string, TextIO.outstream) HashTable.hash_table = HashTable.mkTable (HashString.hashString, op=) (128, Fail "log file not found")
    fun appendToLogFile filename content =
        let
            val fd = case (HashTable.find log_files_ filename) of
                SOME logfd => logfd
                | NONE => let val logfd = TextIO.openAppend filename in HashTable.insert log_files_ (filename, logfd); logfd end
        in
            TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e);
            TextIO.flushOut fd;
            ()
        end

    fun strip to_strip chs =
        let
            fun stripLeft [] chs = []
                | stripLeft (x::xs) chs =
                    if (List.exists (fn c => c = x) chs) then
                        stripLeft xs chs
                    else
                        (x::xs)
            
            val stripped = stripLeft (String.explode to_strip) chs
            val stripped = stripLeft (rev stripped) chs
        in
            String.implode (rev stripped)
        end
    
    fun capitalize s =
        let
            val chs = String.explode s
        in
            case chs of
                x::xs => String.implode ((Char.toUpper x)::xs)
                | _ => s
        end
    
    fun recordName s =
        let
                                
            val parts = String.fields (fn c => c = #"_") s
            val parts = List.filter (fn p => p <> "") parts
        in
            String.concat (List.map capitalize parts)
        end

    fun isSubset [] set = true
        | isSubset (x::xs) set =
            if (List.exists (fn i => x = i) set) then
                isSubset xs set
            else
                false

    fun hasDuplicates [] = false
        | hasDuplicates (x::xs) = 
            if (List.exists (fn i => x = i) xs) then
                true
            else
                hasDuplicates xs

    val urlencode = String.translate (
        fn c => if Char.isAlphaNum c
                then String.str c
                else "%" ^ StringCvt.padLeft #"0" 2
                           (Int.fmt StringCvt.HEX (Char.ord c))
    )

    fun urldecode v =
        let
            val v = String.translate (fn #"+" => " " | c => String.str c) v
            fun process s = let
              val v = Word8.fromString (String.extract (s, 0, SOME 2))
            in
              String.concat [ String.str (Byte.byteToChar (valOf v)),
                              String.extract (s, 2, NONE) ]
            end handle
                Overflow => "%" ^ s
                | Subscript => "%" ^ s
                | Option => "%" ^ s
        in
            String.concat (case String.fields (fn c => c = #"%") v of
                nil => nil
                | x::rest => x::(map process rest))
        end

    fun parseHttpQueryString (query: string): (string*string) list =
        let
            val qparts = String.fields (fn c => c = #"&") query
            val qparts = List.filter (fn p => p <> "") qparts

            fun parse_ [] acc = acc
                | parse_ (q::qs) acc =
                    let
                        val qparts1 = String.fields (fn c => c = #"=") q
                    in
                        if (List.length qparts1) <> 2 then
                            parse_ qs acc
                        else
                            let
                                val k = List.nth (qparts1, 0)
                                val v = List.nth (qparts1, 1)
                            in
                                parse_ qs ((k,v)::acc)
                            end

                    end
        in
            parse_ qparts []
        end

    val GREEN_ = "\027[92m"
    val FAIL_ = "\027[91m"
    val ENDC_ = "\027[0m"
    fun test test_name expecting to_string test_function =
        let
            val _ = print (test_name ^ ": ")
            val res = test_function ()
        in
            if res = expecting then
                print (GREEN_^"OK"^ENDC_^"\n")
            else
                print (FAIL_^"FAIL"^ENDC_^" - expecting '" ^ to_string expecting ^ "' - got '" ^ to_string res ^ "'\n")
        end
        handle Fail msg => print (FAIL_^"EXN"^ENDC_^" Fail(" ^ msg ^ ") - expected '" ^ to_string expecting ^ "'\n")
            | ? => print (FAIL_^"EXN"^ENDC_^" - expected '" ^ to_string expecting ^ "' - " ^ General.exnMessage ? ^ "\n")

end

