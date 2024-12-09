# Apply_bdwgc_proj

I install bdwgc then call GC_MALLOC instead of every malloc. 

In the 'main()' cpp emit, I call GC_INIT(); then GC_gcollect(); and the end.

build! with flag -lgc.

It will run through our same tests but with bdwgc.

# Run executable
./run.sh