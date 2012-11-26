module Main where

{--
        Esqueleto de programa para geração de bubble cloud em Haskell.
        Mais informações em: http://www.inf.ufsm.br/~andrea/elc117-2012b
--}


import System.IO.Unsafe()
import Text.Printf -- Oba, Haskell tem printf! :-)
import Data.List

type Point     = (Float,Float)
type Color     = (Int,Int,Int)
type Circle    = (Point,Float)

imageWidth :: Int
imageWidth = 360

imageHeight :: Int
imageHeight = 360


-- Funcao principal que faz leitura do dataset e gera arquivo SVG
main :: IO ()
main = do 
        strcontent <- readFile infile
        let pairs = map (span (/= ' ')) (lines strcontent)
            freqs = readInts (map snd pairs)
        writeFile outfile (svgCloudGen imageWidth imageHeight freqs)
        putStrLn "Ok!"
        where 
                infile = "dataset.txt"
                outfile = "tagcloud.svg"


-- Transforma lista de strings em lista de inteiros
readInts :: [String] -> [Int]
readInts ss = map read ss


-- Gera o documento SVG da tag cloud, concatenando cabecalho, conteudo e rodape
svgCloudGen :: Int -> Int -> [Int] -> String
svgCloudGen w h dataset = 
        "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n" ++ 
        "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\" \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n" ++
        (svgViewBox w h) ++
        (concat (svgBubbleGen w h dataset)) ++ "</svg>\n"


-- Esta funcao deve gerar a lista de circulos em formato SVG.
svgBubbleGen:: Int -> Int -> [Int] -> [String]
svgBubbleGen w h dataset = [criaTag (fromIntegral w/2) (fromIntegral h/2) (reverse ( sort (lstTransf dataset)))]


-- Gera string representando um circulo em SVG
svgCircle :: Circle -> String
svgCircle ((x,y),r) = printf "<circle cx=\"%f\" cy=\"%f\" r=\"%f\" fill=\"rgb(253,2,0)\" />\n" x y r 

-- Configura o viewBox da imagem e coloca retangulo branco no fundo
svgViewBox :: Int -> Int -> String
svgViewBox w h =
        printf  "<svg width=\"%d\" height=\"%d\" viewBox=\"0 0 %d %d\"" w h w h ++ 
                " version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\">\n" ++
        printf "<rect x=\"0\" y=\"0\" width=\"%d\" height=\"%d\" style=\"fill:white;\"/>\n" w h

-- Raio      
lstTransf :: [Int] -> [Float]
lstTransf [] = []
lstTransf dataset = r : lstTransf(tail dataset)
        where
        raio = fromIntegral (head dataset)/40
        r = raio + 3
       
-- Colisao      
verificaColisao :: Circle -> Circle -> Bool
verificaColisao ((x1, y1), r1) ((x2, y2), r2) = if (dist >= 0.1)
        then True
        else False
        where
             dist = rQ - r1 - r2
             rQ = sqrt (nX + nY)
             nX = (x2 - x1) ^ 2
             nY = (y2 - y1) ^ 2
             
-- Verifica Tag            
verificaTag :: [Circle] -> Circle -> [Bool]
verificaTag [] _ = []
verificaTag lst tag1 = test : (verificaTag (tail lst) tag1)
        where
             test = verificaColisao (head lst) tag1

 
-- configura os pontos na espiral e testa pra ver se não houve colisão
ptCria :: [Circle] -> Float -> Float -> Float -> Point -> [Circle]
ptCria circ t a cR ctr = if (verifica == True)
  then [((cX, cY),cR)]
  else ptCria circ (t + (0.01)) a cR ctr
  where 
        cX = (fst ctr) + (a * t * (cos t))
        cY = (snd ctr) + (a * t * (sin t))
        verifica = and(verificaTag circ ((cX, cY), cR))

-- Cria a lista de tag
criaTag :: Float -> Float -> [Float] -> String
criaTag _ _ [] = []
criaTag x y dataset = criaSVG lstTag
   where
       lstTag = lstCria [((x,y), (head dataset))] 1 0  (tail dataset) (x,y)

-- lista com todos os dados dos pontos das tags
lstCria :: [Circle] -> Float -> Float -> [Float] -> Point-> [Circle]
lstCria tag a t dataset ctr = if (dataset == [])
        then tag
        else (lstCria (tag ++ pN) a 0 (tail dataset) ctr)
        where pN = ptCria tag t a (head dataset) ctr
 
-- envia a lista de tag para o SVG
criaSVG :: [Circle]->String
criaSVG [] = []
criaSVG circl = svgCircle (head circl) ++ criaSVG (tail circl)


        