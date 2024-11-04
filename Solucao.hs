module Solucao where 
    
-- Definição do tipo Coordenadas
    type Coordenadas = (Int, Int, Int)
    type Estado = (Coordenadas, Bool)
    type ID = Int

    
    atualiza_acao :: Bool -> Estado -> Estado
    atualiza_acao acao (posicao, _) = (posicao, acao)

    move :: Coordenadas -> Estado -> Estado
    move (dx, dy, dz) ((x, y, z), ligado)
        | ligado = ((x + dx, y + dy, z + dz), True)
        | otherwise = ((x, y, z), False)

    move_lista :: [Coordenadas] -> Estado -> Estado
    move_lista [] estado = estado -- Se a lista de coordenadas está vazia, não há movimento
    move_lista (m:ms) estado = move_lista ms (move m estado) -- Move recursivamente através da lista de coordenadas

    move_varios :: [([Coordenadas], ID)] -> [Estado] -> [(Estado, ID)]
    move_varios [] [] = []
    move_varios ((movimentos, idNave):naves) (estado:estados) =
        let estadoFinal = move_lista movimentos estado
        in (estadoFinal, idNave) : move_varios naves estados

    verifica_embates :: Estado -> [Estado] -> Bool
    verifica_embates ((x, y, z), _) [] = False
    verifica_embates ((x, y, z), _) (((x', y', z'), _):es)
        | (x, y, z) == (x', y', z') = True -- Se as posições coincidem, há embate
        | otherwise = verifica_embates ((x, y, z), True) es -- Verifica embates com o próximo estado


    move_varios_atualizado :: [([Coordenadas], ID)] -> [Estado] -> [(Estado, ID)]
    move_varios_atualizado [] [] = []
    move_varios_atualizado naves estadosIniciais =
        let 
            -- Calcula os estados finais aplicando todos os movimentos para cada nave
            estadosFinais = zipWith (\(movs, id) estado -> (move_lista movs estado, id)) naves estadosIniciais
            
            -- Extrai apenas as coordenadas finais para verificação
            coordenadasFinais = map fst (map fst estadosFinais)

            -- Função para verificar se existe colisão entre as coordenadas finais
            existeColisao = any (\coord -> length (filter (== coord) coordenadasFinais) > 1) coordenadasFinais

        in if existeColisao
            then zip estadosIniciais (map snd naves) -- Retorna os estados iniciais se houver colisão
            else estadosFinais -- Retorna os estados finais se não houver colisão


            

