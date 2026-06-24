package tip.analysis

import tip.ast._
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.cfg._
import tip.lattices.VarSizeLattice

object VariableSizeAnalysis {

  object Intraprocedural {

    class SimpleSolver(cfg: IntraproceduralProgramCfg)(implicit override val declData: DeclarationData)
        extends IntraprocValueAnalysisSimpleSolver(cfg, VarSizeLattice) {
    }

    class WorklistSolver(cfg: IntraproceduralProgramCfg)(implicit override val declData: DeclarationData)
        extends IntraprocValueAnalysisWorklistSolver(cfg, VarSizeLattice) {
    }
  }
}

