sales_data_vetor <- c(
     "2023-01-01", 1001, "Camisa Social Azul", 5, 50, "Camisas",
     "2023-01-01", 1002, "Calça Jeans", 8, 30, "Calças",
     "2023-01-02", 1003, "Jaqueta de Couro", 2, 20, "Jaquetas",
     "2023-01-03", 1004, "Camisa Social Branca", 3, 45, "Camisas",
     "2023-01-03", 1005, "Saia Longa", 4, 25, "Saias",
     "2023-01-04", 1006, "Blusa de Algodão", 7, 60, "Blusas",
     "2023-01-05", 1007, "Calça de Moletom", 10, 40, "Calças",
     "2023-01-06", 1008, "Vestido Floral", 6, 35, "Vestidos",
     "2023-01-07", 1009, "Terno Completo", 1, 15, "Ternos",
     "2023-01-08", 1010, "Camiseta Básica", 15, 100, "Camisetas"
)
sales_data_vetor

# Criar a matriz a partir do vetor
sales_data_matrix <- matrix(sales_data_vetor, nrow = 10, byrow = TRUE)
sales_data_matrix

# Definir os nomes das colunas
colnames(sales_data_matrix) <- c("Data", "ID_Produto", "Nome_Produto", "Quantidade_Vendida", "Estoque_Disponível", "Categoria")

# Exibir a matriz
sales_data_matrix

sales_data_table <- as.table(sales_data_matrix)
sales_data_table

categorias <- sales_data_matrix[,6]
categorias

frequencias <- table(categorias)
frequencias

path <- "/content/dados_vendas_inventario_meteora_grande (2).csv"

sales_data <- read.csv(path)


class(sales_data)

sales_data_vetor <- c(
     "2023-01-01", 1001, "Camisa Social Azul", 5, 50, "Camisas",
     "2023-01-01", 1002, "Calça Jeans", 8, 30, "Calças",
     "2023-01-02", 1003, "Jaqueta de Couro", 2, 20, "Jaquetas",
     "2023-01-03", 1004, "Camisa Social Branca", 3, 45, "Camisas",
     "2023-01-03", 1005, "Saia Longa", 4, 25, "Saias",
     "2023-01-04", 1006, "Blusa de Algodão", 7, 60, "Blusas",
     "2023-01-05", 1007, "Calça de Moletom", 10, 40, "Calças",
     "2023-01-06", 1008, "Vestido Floral", 6, 35, "Vestidos",
     "2023-01-07", 1009, "Terno Completo", 1, 15, "Ternos",
     "2023-01-08", 1010, "Camiseta Básica", 15, 100, "Camisetas"
)

matriz_sales_data<-matrix(sales_data_vetor, nrow=10, ncol=6,byrow=TRUE)
matriz_sales_data

colnames(matriz_sales_data)<-c('data','ID','nome','qtd_vendida','estoque_disponivel','categoria')
matriz_sales_data

sales_data<-as.table(matriz_sales_data)
sales_data

dados_completos <- read.csv("/content/dados_vendas_inventario_meteora_grande (2).csv")

summary(dados_completos)

head(dados_completos)




camisas_df<- subset(dados_completos, Categoria == "Camisas")
head(camisas_df)

nova_linha <- data.frame(Data = "2023-01-01",
                      ID_Produto = 1001,
                      Nome_Produto = "Camiseta Social Azul",
                      Quantidade_Vendida = -7,
                      Estoque_Disponível = 35,
                      Categoria = "Camisas")


nova_linha

dados_completos<-rbind(dados_completos, nova_linha)

head(dados_completos)

dados_completos[311,]

summary(dados_completos)

dados_completos<- subset(dados_completos, Quantidade_Vendida != -7)

summary(dados_completos)

dados_validos <- subset(dados_completos, Estoque_Disponível >= 0)
print(dados_validos)





total_vendido <- aggregate(Quantidade_Vendida ~ Nome_Produto,data=dados_completos,sum)

media_estoque<- aggregate(Estoque_Disponível ~ Nome_Produto, data=dados_completos, mean)

aggregate_data<- merge(total_vendido, media_estoque, by="Nome_Produto")
print(aggregate_data)

head(aggregate_data)

total_vendas_produto<- tapply(dados_completos$Quantidade_Vendida,dados_completos$Nome_Produto,sum)

print(total_vendas_produto)

columns_to_analyze <- dados_completos[,c("Quantidade_Vendida", "Estoque_Disponível")]

resultado <- sapply(columns_to_analyze, function(x) c(soma = sum(x), media=mean(x), std=sd(x)))
