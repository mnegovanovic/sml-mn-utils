structure MNUtils :> sig
    val println: string -> unit
    val reverse: 'a list -> 'a list
    val test: string -> ''a -> (''a -> string) -> (unit -> ''a) -> unit
end =

struct 
    fun println (s: string): unit =
        print (s ^ "\n")

    fun reverse' [] acc = acc
        | reverse' (x::xs) acc = reverse' xs (x::acc)

    fun reverse l = reverse' l []

    fun test test_name expecting to_string test_function =
        let
            val _ = print (test_name ^ ": ")
            val res = test_function ()
        in
            if res = expecting then
                print "OK\n"
            else
                print ("ERR - expecting '" ^ to_string expecting ^ "' - got '" ^ to_string res ^ "'\n")
        end
        handle Fail msg => print ("EXN Fail(" ^ msg ^ ") - expected '" ^ to_string expecting ^ "'\n")
            | ? => print ("EXN - expected '" ^ to_string expecting ^ "' - " ^ General.exnMessage ? ^ "\n")

end

