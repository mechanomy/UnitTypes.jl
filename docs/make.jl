# 231213: need to ] add ../ to UnitfulTypesDocs package, then run via: /w/sync/mechgits/julia/UnitTypes.jl_public/docs (testItemRunner)$ julia --project make.jl 
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

# =
makedocs(
  sitename="UnitTypes.jl",
  modules=[UnitTypes],
  # root = joinpath(dirname(pathof(UnitTypes)), "..", "docs"),
  root = joinpath( "..", "docs"),
  source = "src",
  build = "build",
  clean=true,
  doctest=true,
  draft=false,
  checkdocs=:all,
  # linkcheck=true, fails to find internal links to bookmarks..
  )

# compile custom theme scss in to css, copying over the default themes
DocumenterTools.Themes.compile("docs/src/assets/themes/documenter-mechanomy.scss", "docs/build/assets/themes/documenter-dark.css")
DocumenterTools.Themes.compile("docs/src/assets/themes/documenter-mechanomy.scss", "docs/build/assets/themes/documenter-light.css")

deploydocs(
  # root = joinpath(dirname(pathof(UnitTypes)), "..", "docs"),
  root = joinpath("..", "docs"),
  target = "build",
  dirname = "",
  repo = "github.com/mechanomy/UnitTypes.jl.git",
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
