# to run: ] add ../#main to UnitfulTypesDocs package, then run via: cd /w/sync/mechgits/julia/UnitTypes.jl_public/docs $ julia --project make.jl 

using Documenter
using DocumenterTools
using DocStringExtensions
push!(LOAD_PATH, "../src/")
# display(LOAD_PATH)
using UnitTypes

# @show pathof(UnitTypes)
# @show dirname(pathof(UnitTypes))
# @show  root = joinpath(dirname(pathof(UnitTypes)), "..", "docs")
# @show  root = joinpath("..", "docs")

@show pkgPath = joinpath(dirname(pathof(UnitTypes)), "..") # this includes UnitTypes/src, so ..

makedocs( # https://documenter.juliadocs.org/stable/lib/public/#Documenter.makedocs
  # repo = Documenter.Remotes.GitHub("mechanomy","UnitTypes.jl"), #"https://github.com/mechanomy/UnitTypes.jl.git",  #this isn't working
  sitename="UnitTypes.jl",
  modules=[UnitTypes],
  root = joinpath(pkgPath, "docs"),
  source = "src",
  build = "build",
  clean=true,
  doctest=true,
  draft=false,
  checkdocs=:none,
  warnonly=Documenter.except(:cross_references, :linkcheck, :missing_docs)
  # linkcheck=true, fails to find internal links to bookmarks..
  )

# compile custom theme scss in to css, copying over the default themes
DocumenterTools.Themes.compile(joinpath(pkgPath,"docs","src","assets","themes","documenter-mechanomy.scss"), joinpath(pkgPath,"docs","build","assets","themes","documenter-dark.css") )
DocumenterTools.Themes.compile(joinpath(pkgPath,"docs","src","assets","themes","documenter-mechanomy.scss"), joinpath(pkgPath,"docs","build","assets","themes","documenter-light.css") )
deploydocs(
  root = joinpath(pkgPath, "docs"),
  target = "build",
  dirname = "",
  # repo = Documenter.Remotes.GitHub("mechanomy","UnitTypes.jl"), #
  repo = "https://github.com/mechanomy/UnitTypes.jl.git",
  branch = "gh-pages",
  deps = nothing, 
  make = nothing,
  devbranch = "main",
  devurl = "dev",
  versions = ["stable" => "v^", "v#.#", "dev" => "dev"],
  forcepush = false,
  deploy_config = Documenter.auto_detect_deploy_system(),
  push_preview = false,
)
# =#
