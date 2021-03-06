#!/bin/bash

# log into the the Viz Docker Container
#docker run -it -v $(pwd):/home/viz -w=/home/viz viz /bin/bash

# first gotta change to test/programs/ to get list of files
chmod 766 test
chmod 766 test/programs/
cd test/programs/
echo
PWD=$(pwd)
echo "Running Viz Test Programs in: $PWD"

# Need to use the Docker Container to run our programs since LLVM is installed in it

# pipe all of the filenames into a txt file, for looping
ls test-*.viz > viz_test_files.txt 
ls fail-*.viz > viz_err_files.txt

# change back to the root to run the ./viz script correctly
cd ../..

# set variables
num_tests=0
num_passed=0
counter=1

# loop through the viz_test_files.txt file to build the .exe files 
# in the _build folder

# Build the C static library of builtin functions.
builtins_ar="viz_builtins.a"
build_dir="_build"
BuildCLib() {
  cd lib/builtins
  make clean
  make
  cp ${builtins_ar} ../../${build_dir}
  cd ../..
}

echo "Building the builtin c static library"
BuildCLib
if [ $? -ne 0 ]; then
    echo "Failed while building the builtin c static library"
    exit;
fi
echo "Done."
echo "Building all of the .viz executables please be patient..."

while IFS= read -r line; do

    BASE=$(echo "$line" | cut -d'.' -f 1) 
    
    FILENAME="test/programs/$BASE.viz"
    
    # build inside the container
    ./vizOutput $FILENAME
    #clear # clear the terminal

done < test/programs/viz_test_files.txt

echo "Done building the executables... "
echo "Now lets run the executables and tests..."
echo

# loop through the viz_test_files.txt file
while IFS= read -r line; do

    BASE=$(echo "$line" | cut -d'.' -f 1) 
    
    FILENAME="test/programs/$BASE.viz"
    REFFILE="test/programs/$BASE.ref"
    OUTFILE="test/programs/$BASE.out"
    EXECUTABLE="$BASE.exe"
    
    # build inside the container
    #./viz $FILENAME
    #clear # clear the terminal

    # run the executable and pipe into .out file
    ./_build/$EXECUTABLE > $OUTFILE 2>> "test/programs/log.txt"

    # get the diff
    run_test=$(diff $OUTFILE $REFFILE)
    if [$run_test = ""]
    then
        echo "Viz Exe Test $counter: $FILENAME passed"
        ((num_passed++))
    else
        echo "Viz Exe Test $counter: $FILENAME failed"
        echo "--------------------------------"
        echo $run_test
        echo "--------------------------------"
    fi

    ((counter++))
    ((num_tests++))
    
done < test/programs/viz_test_files.txt

# loop through the viz_err_files.txt file
while IFS= read -r line; do

    BASE=$(echo "$line" | cut -d'.' -f 1) 
    
    FILENAME="test/programs/$BASE.viz"
    REFFILE="test/programs/$BASE.ref"
    OUTFILE="test/programs/$BASE.out"
    EXECUTABLE="$BASE.exe"
    
    # build inside the container
    #./viz $FILENAME
    #clear # clear the terminal

    # run the executable and pipe into .out file
    # pipe the err message into .out file, and the other garbage into log.txt
    ./vizOutput $FILENAME 2> $OUTFILE >> "test/programs/log.txt"

    # get the diff
    run_test=$(diff $OUTFILE $REFFILE)
    if [$run_test = ""]
    then
        echo "Viz Exe Test $counter: $FILENAME passed"
        ((num_passed++))
    else
        echo "Viz Exe Test $counter: $FILENAME failed"
        echo "--------------------------------"
        echo $run_test
        echo "--------------------------------"
    fi

    ((counter++))
    ((num_tests++))
    
done < test/programs/viz_err_files.txt


# print results back out to the console
echo "($num_passed / $num_tests) Viz Executable Tests Passed"

# remove .out files
echo
echo "removing all the intermediate files in $PWD"
echo

# comment this out to see the intermediary files
#rm test/programs/*.out test/programs/*.tmp test/programs/log.txt test/programs/viz_test_files.txt
rm test/programs/*.out test/programs/log.txt test/programs/viz_test_files.txt test/programs/viz_err_files.txt