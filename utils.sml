structure Utils :> sig
    val println: string -> unit
    val reverse: 'a list -> 'a list
end =

struct 
    fun println (s: string): unit =
        print (s ^ "\n")

    fun reverse' [] acc = acc
        | reverse' (x::xs) acc = reverse' xs (x::acc)

    fun reverse l = reverse' l []
end

