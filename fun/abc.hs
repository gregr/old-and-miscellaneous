import Data.IORef

type Index = Int
type Instruction = MachineState -> MachineState
type ProgramStore = [Instruction]

type GraphNode = IORef Int
type GraphStore = [GraphNode]

type Descriptor = Int
type DescriptorStore = [Descriptor]

type Argument = Int
type ArgumentStack = [Argument]

type BasicValue = Int
type BasicValueStack = [BasicValue]

type Control = Int
type ControlStack = [Control]

data MachineState = MachineState { aStack :: ArgumentStack,
                                   bStack :: BasicValueStack,
                                   cStack :: ControlStack,
                                   graphStore :: GraphStore,
                                   descStore :: DescriptorStore,
                                   progCounter :: Index,
                                   progStore :: ProgramStore
--                                   viewIO :: Int
                                 }

getInstr :: Index -> ProgramStore -> Instruction
getInstr instrId pStore = pStore !! instrId

initProgStore :: [Instruction] -> ProgramStore
initProgStore instrs = instrs
