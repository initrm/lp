(* Funzione ricorsiva per calcolare il fattoriale di un numero n *)
let rec fact(n) = if n<=1 then 1 else n*fact(n-1);;

(* Funzione ricorsiva per calcolare la sequenza di fibonacci, questa soluzione, tuttavia, è molto elegante ma estremamente inefficiente *)
let rec fibo(n) = if n<=1 then n else fibo(n-1) + fibo(n-2);;

(* Funzione iterativa per calcolare la sequenza di fibonacci, estremamente poco elegante ma molto efficiente *)
let fibo´(n) =
    let fib’ = ref 0 and fib’’= ref 1 and fib = ref 1 in
        if n<=1 then n
        else
            (for i=2 to n do
                fib := !fib’ + !fib’’;
                fib’ := !fib’’;
                fib’’ := !fib;
            done;
            !fib);;

(* Funzione con ricorsione in coda, leggermente meno elegante di quella ricorsiva "pura" ma comunque estremamente meglio di quella
iterativa e che gode comunque di ottime prestazioni *)
let rec trfiboaux n m fib_m’ fib_m =
    if (n=m) then fib_m
    else (trfiboaux n (m+1) fib_m (fib_m’+fib_m));;

let fibo´´ n = if n<=1 then 1 else trfiboaux n 1 0 1;;