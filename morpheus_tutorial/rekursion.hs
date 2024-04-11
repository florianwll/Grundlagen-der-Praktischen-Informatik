fakul n = if(n==0) then 1
            else n * fakul (n-1)

fakul2 n = fakulAkku n 1
            where fakulAkku n akku = if(n == 0) then akku else fakulAkku (n-1) (n * akku)

     