module Config (
    blockCount,
    dirtPercent,
    corralDimensions,
    kidCount,
    reactiveRobotCount,
    proactiveRobotCount,
    worldDimension,
    seed,
    maxTime,
    variationTime
    ) where 

-- ***************************************** World Specification *****************************************
blockCount::Int                 
blockCount = 11 

dirtPercent::Int                  
dirtPercent  = 39 

corralDimensions::(Int,Int)
corralDimensions = (5,2)

kidCount::Int
kidCount = 20

reactiveRobotCount::Int
reactiveRobotCount= 1

proactiveRobotCount::Int
proactiveRobotCount = 1

worldDimension::(Int,Int)
worldDimension = (10,10)

seed::Int
seed = 324234

-- **********************************************************************************
-- ***************************************** Simulation Specification *****************************************
maxTime::Int
maxTime = 100


variationTime::Int
variationTime = 500


-- **********************************************************************************