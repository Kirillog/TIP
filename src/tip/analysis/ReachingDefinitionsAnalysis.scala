package tip.analysis

import tip.cfg.IntraproceduralProgramCfg
import tip.ast.AstNodeData.{AstNodeWithDeclaration, DeclarationData}
import tip.lattices.MapLattice
import tip.lattices.PowersetLattice
import tip.cfg._
import tip.ast._
import tip.solvers.SimpleMapLatticeFixpointSolver
import tip.solvers.SimpleWorklistFixpointSolver

abstract class ReachingDefinitionsAnalysis(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData) extends FlowSensitiveAnalysis(true) {

  val lattice: MapLattice[CfgNode, PowersetLattice[AAssignStmt]] = new MapLattice(new PowersetLattice())

  val domain: Set[CfgNode] = cfg.nodes

  NoPointers.assertContainsProgram(cfg.prog)
  NoRecords.assertContainsProgram(cfg.prog)

  def transfer(n: CfgNode, s: lattice.sublattice.Element): lattice.sublattice.Element =
    n match {
      case r: CfgStmtNode =>
        r.data match {
          case as @ AAssignStmt(id: AIdentifier, _, _) =>
            s.filterNot {
              case AAssignStmt(other: AIdentifier, _, _) => other.declaration == id.declaration
              case _ => false
            } + as
          case _ => s
        }
      case _ => s
    }
}

class ReachingDefAnalysisSimpleSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleMapLatticeFixpointSolver[CfgNode]
    with ForwardDependencies

class ReachingDefAnalysisWorklistSolver(cfg: IntraproceduralProgramCfg)(implicit declData: DeclarationData)
    extends ReachingDefinitionsAnalysis(cfg)
    with SimpleWorklistFixpointSolver[CfgNode]
    with ForwardDependencies

