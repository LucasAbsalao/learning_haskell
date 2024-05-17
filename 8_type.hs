-- Exercício
type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa,Livro)]


-- livros emprestados
baseExemplo :: BancoDados
baseExemplo = [("Sergio","O Senhor dos Aneis"),
              ("Andre","Duna"),
              ("Fernando","Jonathan Strange & Mr. Norrell"),
              ("Fernando","Duna")]


-- fazer uma versão usando compreensão de listas e outra usando a função filter
livros1,livros2 :: BancoDados -> Pessoa -> [Livro]
livros1 bd pess = [livr | (p,livr) <- bd, p==pess]
livros2 bd pess = map snd (filter procuraPessoa bd) --map snd (filter (\(p,l) -> (p==pess)) bd)
   where procuraPessoa (p,l) = (p==pess)


-- fazer uma versão usando compreensão de listas e outra usando a função filter
emprestimos1, emprestimos2 :: BancoDados -> Livro ->[Pessoa]
emprestimos1 bd livro = [p | (p,l) <- bd, l==livro]
emprestimos2 bd livro = map fst (filter (\(p,l)->(l==livr)) bd)


emprestado :: BancoDados -> Livro -> Bool
emprestado bd livro = (emprestimos1 bd livro /= [])


qtdEmprestimos :: BancoDados -> Pessoa -> Int
qtdEmprestimos -= length(livros1 bd pess)


emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar bd pess livro = (pess,livro) : bd


devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver bd pess livro = filter (\(p1,l1) -> (p1,l1)==(pess,livro)) bd --Vai excluir todos os pares iguais
