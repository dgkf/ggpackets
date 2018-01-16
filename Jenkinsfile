node {
   stage 'debug'
   locate Rpack

   stage 'build'
   Rscript -e "devtools::build();"
   
   stage 'check'
   Rscript -e "devtools::check();"
   
   stage 'test'
   Rscript -e "devtools::test();"
}