# to run: use the docs/ package
# to serve: cd UnitTypes/docs, > using LiveServer; servedocs(foldername=".") ; this must be done in a WSL or other git terminal, not the devcontainer

using Documenter
using DocumenterTools
using DocStringExtensions
push!(LOAD_PATH, "../src/")

using UnitTypes

# @show pathof(UnitTypes)
# @show dirname(pathof(UnitTypes))
# @show  root = joinpath(dirname(pathof(UnitTypes)), "..", "docs")
# @show  root = joinpath("..", "docs")

@show pkgPath = joinpath(dirname(pathof(UnitTypes)), "..") # this includes UnitTypes/src, so ..

makedocs(
  modules=[UnitTypes],
  sitename="UnitTypes.jl",
  warnonly=[:missing_docs, :cross_references],
  format=Documenter.HTML(assets=["assets/mechanomy.css", "assets/favicon.ico"]),
  clean=true, #clean build/
)

deploydocs( 
  repo = "https://github.com/mechanomy/UnitTypes.jl.git"
)
