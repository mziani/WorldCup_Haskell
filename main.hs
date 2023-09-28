{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import GHC.Generics
import Data.List (sortOn)
import Data.Maybe (catMaybes)
import System.Random (newStdGen, randoms, StdGen)
import Data.List.Split (chunksOf)
import System.Random.Shuffle (shuffleM)
import Data.Set (Set)
import qualified Data.Set as Set

data Team = Team
  { teamName :: String,
    continent :: String,
    rank :: Int,
    score :: Int,
    updateDate :: String
  }
  deriving (Generic, Show, Eq, Ord)

instance FromJSON Team

main :: IO ()
main = do
    -- Charger le contenu du fichier FIFA Ranking
    rankingJson <- B.readFile "fifa-ranking.json"

    -- Analyser le fichier JSON en une liste de Team
    let maybeTeams = decode rankingJson :: Maybe [Team]

    -- Vérifier si l'analyse a réussi
    case maybeTeams of
        Just teams -> do
            let qualifiedTeams = getQualifiedTeams teams

            -- Effectuer le tirage au sort des groupes
            groups <- drawGroups qualifiedTeams

            putStrLn "Liste des 48 équipes qualifiées pour la Coupe du Monde :"
            mapM_ (\team -> putStrLn $ formatTeam team) qualifiedTeams

            putStrLn "\nRésultats du tirage au sort de la phase de groupe :"
            printGroups groups
        Nothing -> putStrLn "Erreur lors de l'analyse du fichier FIFA Ranking."

-- Fonction pour obtenir les 48 équipes qualifiées
getQualifiedTeams :: [Team] -> [Team]
getQualifiedTeams teams =  let -- Filtrer les équipes par continent
        afcTeams = filter (\team -> continent team == "Asia") teams
        cafTeams = filter (\team -> continent team == "Africa") teams
        concacafTeams = filter (\team -> continent team == "Middle and North America") teams
        conmebolTeams = filter (\team -> continent team == "South America") teams
        ofcTeams = filter (\team -> continent team == "Oceania") teams
        uefaTeams = filter (\team -> continent team == "Europe") teams
        
        -- Définir un ensemble pour suivre les équipes déjà sélectionnées
        selectedTeamsSet = Set.fromList []
        
        -- Sélectionner le nombre d'équipes spécifié pour chaque confédération
        selectedAfcTeams = take 8 (sortOn rank $ filter (notInSet selectedTeamsSet) afcTeams)
        selectedCafTeams = take 9 (sortOn rank $ filter (notInSet selectedTeamsSet) cafTeams)
        selectedConcacafTeams = take 6 (sortOn rank $ filter (notInSet selectedTeamsSet) concacafTeams)
        selectedConmebolTeams = take 6 (sortOn rank $ filter (notInSet selectedTeamsSet) conmebolTeams)
        selectedOfcTeams = take 1 (sortOn rank $ filter (notInSet selectedTeamsSet) ofcTeams)
        selectedUefaTeams = take 16 (sortOn rank $ filter (notInSet selectedTeamsSet) uefaTeams)

        -- Équipes supplémentaires pour la CONCACAF (3 équipes hôtes + 1)
        extraConcacafTeams = take 4 (sortOn rank $ filter (notInSet selectedTeamsSet) concacafTeams)

        -- Mettre à jour l'ensemble des équipes sélectionnées
        updatedSet = Set.fromList (selectedAfcTeams ++ selectedCafTeams ++ selectedConcacafTeams ++
                                  selectedConmebolTeams ++ selectedOfcTeams ++ selectedUefaTeams ++
                                  extraConcacafTeams)
        newSelectedTeamsSet = Set.union selectedTeamsSet updatedSet

        -- Concaténer toutes les équipes sélectionnées
        allQualifiedTeams = selectedAfcTeams ++ selectedCafTeams ++ selectedConcacafTeams ++
                            selectedConmebolTeams ++ selectedOfcTeams ++ selectedUefaTeams ++
                            extraConcacafTeams

    in
    -- Trier la liste finale par classement
    sortOn rank allQualifiedTeams

-- Fonction pour formater une équipe pour l'affichage
formatTeam :: Team -> String
formatTeam team = "Nom : " ++ teamName team ++ ", Classement : " ++ show (rank team) ++ ", Continent : " ++ continent team

-- Fonction pour effectuer un tirage au sort de la phase de groupe
drawGroups :: [Team] -> IO [[Team]]
drawGroups qualifiedTeams = do
    -- Générer une graine aléatoire pour le mélange
    gen <- newStdGen

    -- Mélanger la liste des équipes qualifiées de manière aléatoire
    let shuffledTeams = shuffle' qualifiedTeams (length qualifiedTeams) gen

    -- Diviser les équipes en groupes de 4
    let groups = chunksOf 4 shuffledTeams

    return groups


-- Fonction pour afficher les groupes
printGroups :: [[Team]] -> IO ()
printGroups groups = do
    -- Afficher chaque groupe
    mapM_ (\(i, group) -> do
        putStrLn $ "Groupe " ++ show i ++ ":"
        mapM_ (\team -> putStrLn $ "  " ++ teamName team) group
        putStrLn "") (zip [1..] groups)

-- Fonction de mélange en utilisant une graine aléatoire
shuffle' :: [a] -> Int -> StdGen -> [a]
shuffle' xs n gen = take n . map snd . sort . zip rs $ xs
  where
    rs = randoms gen :: [Int]
