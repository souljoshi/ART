# ART
ART - Animation Rendering Tool. (PLT Project)


## Testing

** NOTE: Since semantic check is not implemented, some errors may not be caught by the compiler. **

First checkout the branch you are testing in. If the ``` testall.sh ``` script is not already in 
that file, merge the testing branch.

For example, for testing branch foo:
    ``` git checkout foo ```
    ``` git merge testing ``` [Only if you don't have ``` testall.sh ``` already]


All test files should go in the ``` tests ``` folder.

You will need to create two groups of tests:

1. Programs that are expected to compile. This have the form ``` test-<name>.art ```. For each file ``` test-<name>.art ``` , there should be an associated ``` test-<name>.out ``` which contains the output expected when the program is run.

2. Programs that are expected to fail compilation. This have the form ``` fail-<name>.art ```. These have an associated ``` fail-<name>.err ``` which contains the error message expected when the compilation fails.

Run ``` bash testall.sh ``` to execute the suite. 

A test will have an OK status if does what it is expected to do: pass or fail with the appropriate output. 

If a test fails, the suite will printout the reason for failure. If the failure is caused by difference in output, you can view the corresponding diff file ``` results/test-<name>.diff ``` or ``` results/fail-<name>.diff ```.

You can view the test log in ``` testall.log ```.

Clean the test files with ``` make clean-tests ```.

--> Before testing test files, first make sure they compile by doing the following inside the ART directory
    - make clean
    - make art
    - bash ./compile .tests/name-of-your-test.art ./abc --> if this doesn't fail it compiles!
    - ./abc --> this checks your output.

