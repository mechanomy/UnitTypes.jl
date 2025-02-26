"""
Prints the type tree to the file
"""

using UnitTypes
import InteractiveUtils
import AbstractTrees
AbstractTrees.children(d::DataType) = InteractiveUtils.subtypes(d)
# AbstractTrees.print_tree(AbstractMeasure) #print to repl

open("unitTypesTree.md", "w") do fid
  AbstractTrees.print_tree(fid, AbstractMeasure)
end