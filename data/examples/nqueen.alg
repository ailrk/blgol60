begin
        comment
          -- From the NASE A60 distribution --
                calculation of the prime numbers between 2 and 200
        ;

        integer NN;

        NN := 200;

        begin
                comment first algorithm (check division in a loop) ;

                boolean procedure isprime (n);
                value n;
                integer n;
                begin
                        boolean procedure even (n);
                        value n; integer n; integer i;
                                n := 1;
                                even := entier (n / 2) * 2 = n;

                        isprime := false;
                        if even (n) and n ~= 2 then
                                goto ret;

                        for i := 3 step 2 until n // 2 do
                                if entier (n / i) * i = n then
                                        goto ret;
                        isprime := true;
                    ret:

                end;

                integer i;

                printsln ("first:");

                for i := 2 step 1 until NN do
                        if isprime (i) then
                                printnln (i);
                printsln ("done.")
        end;

        begin
                comment second algorithm (sieve) ;


                boolean array arr [2 : NN];
                integer i, j;

                printsln ("second:");

                for i := 2 step 1 until NN do
                        arr [i] := true;

                for i := 2 step 1 until NN // 2 do
                        for j := 2 * i step i until NN do
                                arr [j] := false;

                for i := 2 step 1 until NN do
                        if arr [i] then
                                printnln (i);
                printsln ("done.")

        end
end
