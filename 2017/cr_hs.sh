ghc -dynamic -o a.out $1_*.hs && {
    echo OUTPUT:
    echo -----------------------
    cat input/$1.txt | ./a.out
    rm *.hi *.o
}
