using TestItemRunner # https://github.com/julia-vscode/TestItemRunner.jl 
@run_package_tests verbose=true
# @run_package_tests verbose=true filter=ti->(occursin("Temperature", ti.filename))
# @run_package_tests verbose=true filter=ti->(occursin("ExchangeUnitful", ti.filename))
# @run_package_tests verbose=true filter=ti->(!occursin("Dimension", ti.filename))
;
