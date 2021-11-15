F1=$1

echo "Test: Evaluate $F1"

cabal -v0 run meta -- $F1 > $F1.output;
./bin/metachk -2 $F1.output $F1.out;

RESULT=$?

rm $F1.output;

if [ $RESULT -eq 0 ] ; then
  echo "Evaluated $1 successfully"  
else
  echo "Failed to Evaluate $1"
fi

exit $RESULT
