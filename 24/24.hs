import qualified Data.Map as Map
import Data.Either (fromRight)
import Data.List (foldl')
import Data.Char (digitToInt)
import Data.Functor ((<&>))

type Value = Int
type Variable = Char

type ALU = Map.Map Variable Value
type ALUFunc = (Value -> Value -> Value)

type FstOperand = Variable
type SndOperand = Either Variable Value

data Command = Input FstOperand
             | Add FstOperand SndOperand
             | Mul FstOperand SndOperand
             | Div FstOperand SndOperand
             | Mod FstOperand SndOperand
             | Eql FstOperand SndOperand
             deriving (Show)

type ModelNumber = String

aluVariables :: [Char]
aluVariables = ['w'..'z']

parseCommand :: String -> Command
parseCommand s = case head splitted of
    "inp" -> Input fstArg
    "add" -> Add fstArg sndArgValue
    "mul" -> Mul fstArg sndArgValue
    "div" -> Div fstArg sndArgValue
    "mod" -> Mod fstArg sndArgValue
    _     -> Eql fstArg sndArgValue
    where splitted    = words s
          fstArg      = head $ splitted !! 1
          sndArg      = head $ splitted !! 2
          sndArgValue = if sndArg `elem` aluVariables
                        then Left sndArg
                        else Right (read (splitted !! 2) :: Int)


parseInput :: IO [Command]
parseInput = map parseCommand . lines <$> readFile "24.in"

initAlu :: ALU
initAlu = Map.fromList $ zip aluVariables $ replicate (length aluVariables)  0

updateAlu :: ALU -> Variable -> Value -> ALU
updateAlu alu var val = Map.insert var val $ Map.delete var alu

computeCommand :: ALU -> ALUFunc -> FstOperand -> SndOperand -> ALU
computeCommand alu f var varOrVal = case varOrVal of
    Left sndVar -> updateAlu alu var (f current (alu Map.! sndVar))
    _           -> updateAlu alu var (f current (fromRight 0 varOrVal))
    where current = alu Map.! var

exec :: ALU -> Command -> ModelNumber -> (ALU, ModelNumber)
exec alu cmd modelNum = case cmd of
    Add fstOp sndOp -> (computeCommand alu (+) fstOp sndOp, modelNum)
    Mul fstOp sndOp -> (computeCommand alu (*) fstOp sndOp, modelNum)
    Div fstOp sndOp -> (computeCommand alu div fstOp sndOp, modelNum)
    Mod fstOp sndOp -> (computeCommand alu mod fstOp sndOp, modelNum)
    Eql fstOp sndOp -> (computeCommand alu (\a b -> if a == b then 1 else 0) fstOp sndOp, modelNum)
    Input fstOp     -> (updateAlu alu fstOp (digitToInt $ head modelNum), tail modelNum)

execCmds :: ALU -> [Command] -> String -> ALU
execCmds alu cmds modelNumber = fst $ foldl' (\(accAlu, accModelNum) cmd -> exec accAlu cmd accModelNum) (initAlu, modelNumber) cmds

isValidModelNum :: ALU -> [Command] -> ModelNumber -> Bool
isValidModelNum alu commands modelNumber = computed Map.! 'z' == 0
    where computed = execCmds initAlu commands modelNumber

findHighestValid :: Integer -> [Command] -> IO Integer
findHighestValid modelNumber commands
    | '0' `elem` show modelNumber = findHighestValid (modelNumber - 1) commands
    | isValidModelNum initAlu commands $ show modelNumber = return modelNumber
    | otherwise = do
        print modelNumber
        findHighestValid (modelNumber - 1) commands

fstPart :: IO Integer
fstPart = do
    commands <- parseInput
    findHighestValid 80000000000000 commands
        