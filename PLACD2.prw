#include "protheus.ch"
#include "apvt100.ch"
#include "topconn.ch"
#include "rwmake.ch"
#include "tbiconn.ch"
#include "ap5mail.ch" 
#include "RPTDEF.CH"

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// @Title: Picklist de Saï¿½da - MS AMBROGIO                                                                                //                                                                   
// @Desc:                                                                                                               //                                    
// @Autor: Arinus K. - Compton                                                                                                  //                                      
// @Date: 14/03/2022                                                                                                 //   
// @Adicional: Ponto de entrada Etiqueta + Relatorio Cadsz7 + Browser Protheus CADSZ7 + Etiqueta impressao Manual;   //                                 
//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

User Function PLACD2()
    Local  i
    Public aTmpNms  := {}
    Public cItemAt  := ""     //Item Atual
    Public nVolTotl := 0.0000 //somatoria volumes
    Public nPlTotl  := 0.0000 //somatï¿½ria peso lï¿½quido
    Public nPbTotl  := 0.0000 //somatória peso bruto
       
    Private nOp := 0

    Public lCntCInc := .F. //Contar caixas incompletas? (volume e qnt)

    Public lAddProd := .T. //Se o produto pode ser adicionado. Para casos em q a qnt é 0.0000 no momento de beepar, nao adiciona a linha ao Z8Itens.


    @ 0,0 VTSAY "Selecione Uma Opcao:"
    nOp:=VTaChoice(1,0,6,VTMaxCol(),{"Incluir Picklist","Visualizar Picklist"})
    
    If nOp == 1
        //Funï¿½ï¿½o Inicial do Picklist de Saï¿½da
        VTCLEAR()
        U_INCPLS()
    Elseif nOp == 2
        U_VISUPLS() //VISUALIZAR PICKLIST
    Endif

    //Fecha todos os alias temporarios em aberto, utilizando a array aTmpNms.
    //Evita o erro Alias Aready in use
    
    ClsTmp()

Return

/////////////////////////////////////////////
// Função Visualizar Picklist de Saída   //
///////////////////////////////////////////

Static Function U_VISUPLS()
    //Contadores
    Local q

    //Arrays
    Private aVisuVB :=  {}
    Private aItens  :=  {}
    Private aZ8It   :=  {}
    Private aItensZ8 := {}

    //Outros
    Private lExit   := .F.
    Private nOpyw   := 0


    While !lExit

        //Cabeçalho
        IF SELECT("SZ7") > 0
            SZ7->(Dbclosearea())
        ENDIF

        //Numero,Data,Hora,Solicitante,Natureza Op, Num Pedido, Nota Fiscal, Qnt Volume.
        cQuerX := "SELECT * FROM "+RetSqlName("SZ7")+" WHERE Z7_STATUS='A' AND D_E_L_E_T_='' "
        TCQUERY cQuerX NEW ALIAS "SZ7"
        TCSETFIELD("SZ7","Z7_DATA","D",8,0)

        DbSelectArea("SZ7")
        SZ7->(DbGoTop())
        
        While !SZ7->(Eof())
            Aadd(aVisuVB, {SZ7->Z7_NUM,Stod(DtoS(SZ7->Z7_DATA)),SZ7->Z7_HORA,SZ7->Z7_SOLIC,SZ7->Z7_NATOP,SZ7->Z7_PEDIDO,SZ7->Z7_NOTA,SZ7->Z7_QVOL})
            SZ7->(DbSkip())
        Enddo

        nMaxIncv := len(aVisuVB)
        aItPl := Array(nMaxIncv, 8) //cria array para montagem do menu VTABROWSE.

        For q:=1 To nMaxIncv
            aItPl[q] := {aVisuVB[q,1],aVisuVB[q,2],aVisuVb[q,3],aVisuVB[q,4],aVisuVB[q,5],aVisuVB[q,6],aVisuVB[q,7],aVisuVB[q,8]}
            Aadd(aItens,aItPl[q])
        Next

        If nMaxIncv == 0
            //Caso nao exista itens no picklist SZ7
            VTALERT("Nada retornou da consulta SQL SZ7. SOLUÇÃO: Verifique se existem picklists cadastrados.")
            Return
        Endif

        aCab := {"Num","Data            ","Hora      ","Solicitante                            ","Natureza Op   ","Num Pedido         ","Nota F.        ","Qnt Volume                  "} 
        aSize:= {TamSx3("Z7_NUM")[1],TamSx3("C6_ENTREG")[1],TAMSX3("Z7_HORA")[1],TAMSX3("Z7_SOLIC")[1],7,TamSx3("Z7_PEDIDO")[1],TamSx3("Z7_NOTA")[1],TamSx3('Z7_QVOL')[1]}  
        nPos := 1 
        nChoose  := VTaBrowse(0,0,6,31,aCab,aItens,aSize,,len(aItens))

        VTCLEAR()

        //VTAlert("Voce escolheu o picklist "+aVisuVB[nChoose,1])
        
        cPlChsd := aVisuVB[nChoose,1]

        //Puxa itens de acordo com o picklist escolhido.
        If Select("SZ8")>0
            SZ8->(DbCloseArea())
        EndIf

        //Numero,Data,Hora,Solicitante,Natureza Op, Num Pedido, Nota Fiscal, Qnt Volume.
        cQuerX := "SELECT * FROM "+RetSqlName("SZ8")+" WHERE Z8_NUM='"+cPlChsd+"' AND D_E_L_E_T_='' "
        TCQUERY cQuerX NEW ALIAS "SZ8"
        TCSETFIELD("SZ8","Z8_DATENT","D",8,0)

        DbSelectArea("SZ8")
        SZ8->(DbGoTop())

        While !SZ8->(Eof())
            aAdd(aZ8It,{SZ8->Z8_ITEM,SZ8->Z8_CODMP,SZ8->Z8_DESC,SZ8->Z8_UM,SZ8->Z8_QUANT,SZ8->Z8_LOCALIZ,SZ8->Z8_LOTE,SZ8->Z8_PESOL,SZ8->Z8_PESOB,SZ8->Z8_QCXC,SZ8->Z8_QNTINC,Stod(DtoS(SZ8->Z8_DATENT))}) 
            SZ8->(DbSkip())
        EndDo

        nMaxIt := len(aZ8It)

        aItb := Array(nMaxIt, 13) //cria array para montagem do menu VTABROWSE.

        For q:=1 To nMaxIt
            aItb[q] := {aZ8It[q,1],aZ8It[q,2],aZ8It[q,3],aZ8It[q,4],Transform(aZ8It[q,5],"@R 99999999999.9999"),aZ8It[q,6],aZ8It[q,7],Transform(aZ8It[q,8],"@E 9,999,999.999"),Transform(aZ8It[q,9],"@E 9,999,999.999"),aZ8It[q,10],Transform(aZ8It[q,11],"@E 9,999,999.999"),aZ8It[q,12]}
            Aadd(aItensZ8,aItb[q])
        Next

        //Caso o picklist estiver sem itens
        If nMaxIt != 0 
            aCab := {"Item","Produto            ","Desc         ","UM  ","Quant      ","Endereco         ","Lote        ","Peso Liq.                  ","Peso Brut.         ","Caixas Com      ","Caixas Inc.      ","Data Ent       "} 
            aSize:= {TamSx3('Z8_ITEM')[1],TamSx3('Z8_CODMP')[1],TamSx3('Z8_DESC')[1],TamSx3('Z8_UM')[1],TamSx3('Z8_QUANT')[1]+2,TamSx3('Z8_LOCALIZ')[1],TamSx3('Z8_LOTE')[1],TamSx3('Z8_PESOL')[1]+2,TamSx3('Z8_PESOB')[1]+2,TamSx3('Z8_QCXC')[1],TamSx3('Z8_QNTINC')[1]+6,8}  
            nPos := 1 
            @ 0,0 VTSAY "Itens - PL "+alltrim(cPlChsd)
            nChoose  := VTaBrowse(1,0,6,31,aCab,aItensZ8,aSize)
            VTCLEAR()
            @ 0,0 VTSAY "Opcoes:"
            nOpyw:=VTaChoice(1,0,6,VTMaxCol(),{"1-Outro Picklist","2-Sair"})
            If nOpyw == 1
                //Reset das variaveis do browser
                aItens   := {}
                aItensZ8 := {}
                aZ8It    := {}
                aItb     := {}
                aItPl    := {}
                VTCLEAR()

            Elseif nOpyw == 2
                //Sair do visualizar picklist
                VTALERT("Finalizando..","Finalizando",.T.,500)
                Return
            Endif
        else
            VTALERT("Nada retornou da consulta SQL. SOLUÇÃO: Verifique se existem itens cadastrados no picklist.")
            VTCLEAR()
        Endif



    Enddo
    

Return

////////////////////////////////////////////
// Funï¿½ï¿½o Incluir Picklist de Saï¿½da /
//////////////////////////////////////////
Static Function U_INCPLS()

    //For
    Local q := 1
    Local y := 0
    Private nQntPend := 0
    Private nPesoBli := 0 //Peso Bruto Linha (peso bruto = pesob*numcaixas)
    Private nPesoLli := 0 //Peso Liquido Linha
    Private aItx     := {}
    Private aChsedIt := {}
    
    Private nContlp :=  0
    
    Private cItsel  := ""
    Private nOpc    :=  0
    Private cPvenda := SPACE(6)  //preenche com espaï¿½os em branco de acordo com tamanho do campo
    
    //Arrays:
    Private aIncVB    := {} //MONTAGEM TELA VTABROWSE
    Private aPedVenda := {} //MONTAGEM TELA VTABROWSE
    Private aItens    := {} //MONTAGEM TELA VTABROWSE
    Private aAddsys   := {} //ITENS SELECIONADOS P/ INCLUSAO DB
    Private aSugTela  := {} //MONTAGEM TELA SUGESTï¿½ES
    Private aMntSug   := {} //MONTAGEM TELA SUGESTï¿½ES
    Private aItensX   := {} //MONTAGEM TELA SUGESTï¿½ES 
    Private aReadyad  := {} //Array com os itens já adicionados
    Private aQuery    := {} //Array Query Qnt Pendente
    
    //Públicas:
    Public aZ8Itens   := {} //array para montar tela com o adicionado - CODMP,DESC,UM,ARMZ,LOTE,LOCALIZ,QNT
    Public aZ8Insrt   := {} //array final - array que serï¿½ passada a database.
    Public nZ8ItSq    := 01 //contagem sequencial dos itens no Z8.


    //Boleanas
    Private bArdyAd := .F.
    Private bSelag  := .T.
    Private bEmpty
    Private bCanAdd := .F. //Permitir ou não adicionar o item a tela final.

    Private lPular  := .F. //Correção bug ao escolher não.
    //Outros
    Private nNatOp := "" //Natureza da Operação
    
    @ 0,0 VTSAY "Selecione a Natureza da OP:"
    nNatOp:=VTaChoice(1,0,6,VTMaxCol(),{"Venda","Beneficiamento","Devolucao","Retrabalho","Outras Saidas"})
    VTCLEAR()

    //Natureza da Op
    //Tipos:   V=Venda;B=Beneficiamento;D=Devolução;R=Retrabalho;O=OutrasSaídas;

    If nNatOp == 1    //venda
        cNatOp := "V"
    Elseif nNatOp == 2
        cNatOp := "B" //beneficiamento
    Elseif nNatOp == 3
        cNatOp := "D" //devolução
    Elseif nNatOp == 4
        cNatOp := "R" //retrabalho
    Elseif nNatOp == 5
        cNatOp := "O" //Outras saídas
    Endif   

    DbSelectArea("SZ5")
    DbGoBottom()
    //ADICIONA SZ7 CABEï¿½ALHO
    
    //->Numero Picklist
    cNumSoli := GETSXENUM("SZ7","Z7_NUM") 
    
    //->Data Pedido
    dDatasol := Date()
    cDatasol := DToC(dDatasol)

    //->Hora do Pedido
    tTimesol := Time()

    //->Solicitante do Pedido
    cUsrsol := FwGetUserName(RetCodUsr())

    //->Status
    cStatus := "A" //inicia status como aberto

    DbCloseArea()

    @ 0,0 VTSAY "Insira o Numero do PdV:"
    @ 2,0 VTSAY "PdV:" VTGET cPvenda  VALID ValPVenda(cPVenda)==.T.
    VTREAD
    VTCLEAR()
    


    
    IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF
      
    cQry := "SELECT * FROM "+RetSqlName("SC6")+" WHERE C6_NUM='"+cPVenda+"' AND D_E_L_E_T_=''" 
    TCQUERY cQry NEW ALIAS "TMP"

    //cTst := TMP->C6_QTDVEN - TMP->C6_QTDLIB
    
    //VTALERT("Qnt Pendente:"+str(cTst))

    DbSelectArea("TMP")
    TMP->(DbGoTop())

    While !TMP->(Eof())

        //*Qnt de Venda - Qnt Entregue (faturada sd2)
        nQntPend = TMP->C6_QTDVEN-TMP->C6_QTDENT

        //aparecer itens do pedido com qnt_pendente > 0
        If nQntPend > 0
            Aadd(aIncVB, {TMP->C6_ITEM,TMP->C6_NUM,TMP->C6_PRODUTO,TMP->C6_ENTREG,nQntPend})
        Endif



        TMP->(DbSkip())
    Enddo

    TMP->(dbclosearea())


    nMaxIncv := len(aIncVB) //puxa o tamanho da array para delimitar o laï¿½o For.

    aPedVend := Array(nMaxIncv, 4) //cria array para montagem do menu VTABROWSE.


    For q:=1 To nMaxIncv
        //Transform(Alltrim(aIncVB[q,5]),"@R 99999999999.9999")
        aPedVend[q] := {(aIncVb[q,1]),alltrim(aIncVB[q,2]),alltrim(aIncVB[q,3]),dToC(StoD(aIncVB[q,4])),Transform(aIncVB[q,5],"@R 99999999999.9999")}
        Aadd(aItens,aPedVend[q])
    Next

    //VTALERT("aIncvb,q,4:"+cValToChar(aIncVB[1,4]))

    If nMaxIncv >0
        VTALERT("Selecione as linhas para seguir com o processo de expedicao","Aviso",.T.,1250)
        aCab := {"It","PdV            ","Produto      ","Data Entrega                            ","Qnt Pend.   "} 
        //codmp 12,lote 10,end 30, saldo 15
        aSize:= {TamSx3("C6_ITEM")[1],TamSx3("C6_NUM")[1],TAMSX3("C6_PRODUTO")[1],TAMSX3("C6_DATFAT")[1],18}   
        nPos := 1 
        While bSelag
            lPular := .F. //reset variavel lPular
            VTCLEAR()
            aCab := {"It","PdV            ","Produto      ","Data Entrega                            ","Qnt Pend.   "} 
            //codmp 12,lote 10,end 30, saldo 15
            aSize:= {TamSx3("C6_ITEM")[1],TamSx3("C6_NUM")[1],TAMSX3("C6_PRODUTO")[1],TAMSX3("C6_DATFAT")[1],18}   
            nPos := 1 
            nChoose  := VTaBrowse(0,0,6,31,aCab,aItens,aSize)
            
            //cDtEnt := dToC(StoD(aIncVB[nChoose,4]))
            
            //MsgAlert("DtEnt:"+dToC(StoD(aIncVB[nChoose,4])))

            VTCLEAR()
            
            //checa se o usuario já selecionou o item do pedido de venda
            If CheckDup(StrZero(nChoose,2),aReadyad)
                cItemAt := strzero(nChoose,2)
                VTAlert("O item "+StrZero(nChoose,2)+" ja foi adicionado. Selecione um item valido.","Erro")
                bCanAdd := .F.
            Else
                cItemAt := strzero(nChoose,2)
                bCanAdd := .T.
                aAdd(aReadyad,StrZero(nChoose,2)) //Log dos itens adicionados
            Endif

            //Realiza consulta Sql Lote x Endereço e adiciona ao menu final.
            //Caso o item for duplicado, pula para "adicionar mais itens" e nao exibe menu de adicionados.
            If bCanAdd

                VTCLEAR()

                //ITEM,CODMP,DESC,UM,ARMZ,LOTE,LOCALIZ,QNT   
                cCodMp := aIncVB[nChoose,3]
 
                cItemOrig  := aIncVB[nChoose,1] //qual item do pedido de venda originou a linha

                //Verifica se tabela para o produto já foi criada. Caso sim, salva a posição para uso no restante da rotina.
                //Caso nao, cria tabela e adiciona o log a aTmpNMS.
                nPosTble := ASCAN(aTmpNms,{|x| x[1]== alltrim(cCodMP) })

                //Passa nome do Alias da tabela temporaria referente ao produto
                //Alias sempre TMP_Produto
                cAliasNam := "TMP_"+alltrim(cCodMP)

                //A query real popula a tabela temporaria.
                //as atualizações posteriores são feitas em cima da temporaria, e encerradas no momento 
                //de fechamento da rotina.
                If nPosTble > 0
                    //se tabela existir, puxa nome e insere em cTableEx
                    cTableEx := aTmpNms[nPosTble,2]
                Else
                    //se tabela nao existir..
                    //cria tabela e adiciona o nome real a array
                    //aTmpNMS := {Produto,Tabela,Alias}
                    //VTALERT("A tabela nao existe.. Criando e adicionando log")
                    aAdd(aTmpNms,{alltrim(cCodMP),LxeTmp(cCodMP),cAliasNam})
                    nPosTble := ASCAN(aTmpNms,{|x| x[1]== alltrim(cCodMP) })
                    cTableEx := aTmpNms[nPosTble,2]
                Endif

                //Puxando Desc
                DbSelectArea("SB1")
                SB1->(DbSetOrder(1)) //FILIAL+COD
                If SB1->(DbSeek(xFilial("SB1")+cCodMP))
                    //Puxa Desc
                    If !Empty(SB1->B1_DESC)
                        cDesc := SB1->B1_DESC
                    else
                        cDesc := ""
                    Endif
                    //Puxa UM
                    If !Empty(SB1->B1_UM)
                        cUm := SB1->B1_UM
                    Else
                        cUm := ""
                    Endif

                Endif
                SB1->(DbCloseArea())

                //nQntprv := val(aIncVB[nChoose,5]) //qnt prevista
                
                nQntprv := Val(Transform(aIncVB[nChoose,5],"@R 99999999999.999"))
                
                nMxEstoq := LxEMxTmp(cCodMp)                

                //Puxa sql da tela de acordo com o produto LxEMxTmp(cCodMP)
                VTALERT("Qnt prev:"+str(nQntprv)+"-MxEstoq:"+str(nMxEstoq),"Estoque")

                //Ativar contagem de caixas incompletas?
                @ 0,0 VTSAY "Consid. Caixas Inc?"
                nOpy:=VTaChoice(1,0,6,VTMaxCol(),{"Sim","Nao"})
                If nOpy == 1
                    lCntCInc := .T.
                    VTALERT("Carregando..","Aguarde",.T.,300)
                Else
                    lCntCInc := .F.
                    VTALERT("Caixas incompletas nao serao consideradas(vol,qnt)","Aviso",.T.,2000)
                Endif

                //Monta tela de acordo com o Lote x Endereço.
                If nQntprv>nMxEstoq
                    //Se o nMxEstoque estiver zerado, apenas avisa o usuario de que nao ha nenhum saldo
                    If nMxEstoq != 0
                        //nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                        VTCLEAR()
                        @ 0,0 VTSAY "Quantidade total n/ disponivel em estoque."
                        @ 1,0 VTSAY "Adicionar o total disponivel?"
                        nOp:=VTaChoice(2,0,6,VTMaxCol(),{"Sim","Nao"})
                        VTALERT("Carregando..","Aguarde",.T.,500)
                        If nOp == 1                            
                            bArdyAd := .T.

                            lPular  := .F.
                            //Puxando Desc e UM de acordo com o código de produto
                            If Select("SB1") > 0
                                SB1->(DbCloseArea())
                            Endif

                            DbSelectArea("SB1")
                            DbSetOrder(1)
                            IF SB1->(DbSeek(xFilial("SB1")+cCodMP))
                                //DESC
                                If !empty(SB1->B1_DESC)
                                    cDesc := SB1->B1_DESC
                                Else
                                    cDesc := ""
                                Endif

                                //Um
                                If !empty(SB1->B1_UM)
                                    cUm := SB1->B1_UM
                                Else
                                    cUm := ""
                                Endif

                                //Qnt Emb
                                If !empty(SB1->B1_QE)
                                    nQntEmb := SB1->B1_QE
                                Else
                                    nQntEmb := 0
                                    //VTALERT("Produto sem QntEmb cadastrada.","Aviso",.T.,1000)
                                Endif
                            Endif


                            (cAliasNam)->(DbGoTop())

                            While !(cAliasNam)->(Eof())
                                //TMP := FILIAL,LOCAL,PRODUTO,LOTE,LOCALIZ,SALDO
                                bEmpty := .T.
                                
                                //Trava para nao pegar linha com saldo == 0 
                                If (cAliasNam)->SALDO == 0
                                    (cAliasNam)->(DbSkip())
                                Else
                                    
                                    //Separando campos por var
                                    cArmz    := (cAliasNam)->LOCALX //BF_LOCAL
                                    cCodMP   := (cAliasNam)->PRODUTO //cCodMP
                                    cLote    := (cAliasNam)->LOTE //B8_LOTECTL
                                    cEnd     := (cAliasNam)->LOCALIZ //BF_LOCALIZ
                                    cSaldoli := (cAliasNam)->SALDO //B8_SALDO

                                    If BeepValid(cCodMP,cSaldoli,cLote,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                                        nNumcx  := NoRound(cSaldoli / nQntEmb , 0)   //caixas completas
                                        nCxInc  := Mod(cSaldoli,nQntEmb )           //caixas incompletas

                                        nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                                        nPesoLli := nTmpPliq * nNumcx //total peso liq
                                        
                                        //Caixa incompleta:
                                        //1- Se existir no calculo;
                                        //2- Se habilitado pelo usuario;
                                        //* Se as duas condições forem válidas, soma no volume e na quantidade.
                                        //* Se houver caixas incompletas
                                        If nCxInc > 0 .AND. lCntCInc
                                            nIncVol := 1 
                                            nPesoBli += (nTmpPbrt * nCxInc)/ nQntEmb
                                            nPesoLli += (nTmpPliq * nCxInc)/ nQntEmb

                                        Else
                                            nIncVol := 0
                                        EndIf
                                        
                                        
                                        //* lCntcInc := Se contagem caixa incompleta estiver ativado
                                        If lCntCInc
                                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,cLote,cEnd,cSaldoli,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                                        Else

                                            //* Se caixa incompleta estiver desativada, subtrai caixa inc 
                                            If Empty(nCxInc)
                                                nCxInc := 0
                                            Endif
                                            
                                            if lAddProd != .F.
                                                Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,cLote,cEnd,cSaldoli-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                                            Endif
                                        Endif
                                    
                                        nVolTotl := nVolTotl + nNumCx + nIncVol
                                        nPlTotl  := nPlTotl+nPesoLli
                                        nPbTotl  := nPbTotl+nPesoBli

                                        //Pltotal = soma peso linha * vol

                                        //Zera linha do saldo, pois toda quantidade disponivel foi 
                                        //usada para compor parte da qntprev.
                                        //Como o saldo disponivel é inferior a quantidade prevista no pedido de venda,
                                        //podemos zerar o saldo de cada linha, pois toda quantidade da linha será usada.
                                        
                                        (cAliasNam)->SALDO := 0
                                
                                    Endif
                                    (cAliasNam)->(DbSkip())
                                Endif
                                
                            Enddo
                        Else
                            lPular := .T.
                            //VTALERT("Aguarde..","Carregando",.T.,500)
                        Endif
                    Else
                        VTALERT("Nao existe saldo em estoque para atender o produto "+alltrim(cCodMP)+". O saldo em estoque é "+alltrim(str(nMxEstoq)),"Sem Estoque")
                    EndIf
                Endif

                //Lï¿½gica Adicionar Estoque Automaticamente de Acordo com o Lote x Endereï¿½o
                //1- Puxa Query Lote x End com filtro validade lote.
                //2- Pega linhas atï¿½ que o Qnt a faturar seja batido. Caso o estoque total disponï¿½vel nao seja
                //suficiente, passa todas as linhas.
                //Qntlinha = Qnt de saldo no Lote x Endereï¿½o na linha especificada.
                //Qnt prev = Qnt de saldo previsto p/ faturamento no Pedido de Venda.
                //Caso nao tenha sido adicionado na tela de sem estoque disponivel, adiciona 
                //agora.

                //MsgAlert("lPular"+cValToChar(lPular))
                If !lPular
                    //MsgAlert("lPular"+cValToChar(lPular))
                    If bArdyAd == .F.  
                        LxEDistr(cCodMP,nQntprv,cAliasNam,cItemOrig)  
                    else
                        bArdyAd := .F.
                    Endif

                    //Reset lPular
                    lPular := .F.    

                EndIf
                TelaSZ8(aZ8Itens) //monta tela com os itens desmembrados

            Endif            



            //BeepQr(aZ8Itens) //Tela para beepar itens pelo QR ou Manual.
            If !bCanAdd
                @ 0,0 VTSAY "Tentar outro item?"
            Else
                @ 0,0 VTSAY "Adicionar mais itens ?"
            Endif
        
            nOpy:=VTaChoice(1,0,6,VTMaxCol(),{"Sim","Nao"})
            
            If nOpy == 1
                bSelag := .T.
                //Aadd(aAddsys, {(aIncVb[nChoose,1]),alltrim(aIncVB[nChoose,2]),alltrim(aIncVB[nChoose,3]),alltrim(aIncVB[nChoose,4]),alltrim(aIncVB[nChoose,5])}) //ADICIONA ARR ITENS Q SERAO INCLUIDOS
                if bEmpty
                    nContlp++
                Endif
                bEmpty := .F.
                nZ8ItSq++
            Elseif nOpy == 2
                bSelag := .F.

            
                @ 0,0 VTSAY "Confirma a inclusao?"
                nOpcy:=VTaChoice(1,0,3,VTMaxCol(),{"Sim","Nao"})
                if nOpcy == 1
                    If len(aZ8Itens) != 0
                        aZ8Insrt := aClone(aZ8Itens)    //aZ8Insrt copia da aZ8Itens

                        //Incluindo Cabeï¿½alho
                        RECLOCK("SZ7",.T.) ///// .T. QDO INCLUSÃO, .F. QNDO ALTERAÇÃO
                        REPLACE SZ7->Z7_FILIAL WITH FWFilial()
                        REPLACE SZ7->Z7_NUM WITH cNumsoli
                        REPLACE SZ7->Z7_DATA WITH Date()
                        REPLACE SZ7->Z7_SOLIC WITH cUsrsol
                        REPLACE SZ7->Z7_HORA WITH Time()
                        REPLACE SZ7->Z7_STATUS WITH cStatus
                        REPLACE SZ7->Z7_PEDIDO WITH aIncVB[1,2] //1 pq a variavel ï¿½ preenchida com smnt 1 pdv
                        REPLACE SZ7->Z7_QVOL WITH nVolTotl
                        REPLACE SZ7->Z7_PESOL WITH nPlTotl
                        REPLACE SZ7->Z7_PESOB WITH nPbTotl 
                        REPLACE SZ7->Z7_NATOP WITH cNatop

                        SZ7->(MSUNLOCK())

                        //ADICIONA SZ8 ITENS
                        nMxItens := len(aZ8Insrt) //pega total de itens selecionados

                        //z8itens{codmp,desc,um,arm,lote,end,qnt}
                        //1 cCodMp,2 cDesc,3 cUm,4 cArmz 05,TMP->B8_LOTECTL 06,TMP->BF_LOCALIZ 07,TMP->B8_SALDO 08,nTmpPliq 09,nTmpPbrt10,nNumCx 11,nCxInc 12
                        For q:= 1 To nMxItens
                            //7=Qnt, 11=Qnt caixa incompleta, 14=Considerar Inc?
                            //Se nao houver caixas completas e houver caixa incompleta e Considerar Inc == .F.

                            RECLOCK("SZ8",.T.) ///// .T. QDO ï¿½ INCLUSï¿½O, .F. QDO ï¿½ ALTERAï¿½ï¿½O
                            REPLACE SZ8->Z8_FILIAL WITH FWFilial()
                            REPLACE SZ8->Z8_NUM WITH cNumsoli
                            REPLACE SZ8->Z8_ITEM WITH StrZero(q,2)
                            REPLACE SZ8->Z8_CODMP WITH Upper(aZ8Insrt[q,1])
                            REPLACE SZ8->Z8_DESC WITH aZ8Insrt[q,2]
                            REPLACE SZ8->Z8_UM WITH aZ8Insrt[q,3]
                            REPLACE SZ8->Z8_ARMZ WITH aZ8Insrt[q,4]
                            REPLACE SZ8->Z8_QUANT WITH aZ8Insrt[q,7]
                            REPLACE SZ8->Z8_LOTE WITH Upper(aZ8Insrt[q,5])
                            REPLACE SZ8->Z8_LOCALIZ WITH Upper(aZ8Insrt[q,6])
                            REPLACE SZ8->Z8_PESOL WITH aZ8Insrt[q,8]
                            REPLACE SZ8->Z8_PESOB WITH aZ8Insrt[q,9]
                            REPLACE SZ8->Z8_QCXC WITH aZ8Insrt[q,10]
                            REPLACE SZ8->Z8_QNTINC WITH aZ8Insrt[q,11]
                            

                            REPLACE SZ8->Z8_ITPDV WITH aZ8Insrt[q,12]
                            REPLACE SZ8->Z8_DATENT WITH STod(aZ8Insrt[q,13])

                            //Salva qual a escolha do usuário no input da linha ACD.
                            If  aZ8Insrt[q,14]
                                REPLACE SZ8->Z8_OPINC WITH "Sim"
                            Else
                                REPLACE SZ8->Z8_OPINC WITH "Nao"
                            Endif

                            //1 cCodMp,2 cDesc,3 cUm,4cArmz,5 TMP->B8_LOTECTL,6 TMP->BF_LOCALIZ,7 TMP->B8_SALDO,8 nTmpPliq,nTmpPbrt,nNumCx,nCxInc
                            SZ8->(MSUNLOCK())
                
                        Next 

                        ConfirmSx8()
                        VTCLEAR()
                        VTALERT("Pl Saida num "+cNumSoli+" adicionado.","Sucesso",.T.,4000)

                        //Apï¿½s adicionar, envia a notificaï¿½ï¿½o automï¿½tica via e-mail
                        cData := DtoC(Date()) //data
                        cHora := Time() //hora

                        VTALERT("Enviando Notificacao Automatica via E-mail..","Aguarde",.T.,4000)
                        //MsgRun("Enviando Notificação Automática ao Dpto Fiscal..","Aguarde...",{|| InKey(4),AutoEml(cNumsoli,cData,cHora,cUsrsol,aIncVB[1,2],aZ8Insrt,cStatus) })
                        
                        AutoEml(cNumsoli,cData,cHora,cUsrsol,aIncVB[1,2],aZ8Insrt,cStatus)
                        
                        //Obs: as tabelas temporarias sao encerradas na função principal.
                    Else
                        VTALERT("O picklist nao pode ser adicionado. MOTIVO: Nenhum item adicionado.") 
                    Endif

                Elseif nOpcy ==2
                    RollbackSx8()
                    VTALERT("Abortado pelo usuario. Inclusao nao realizada","Finalizando",.T.,4000)
                    //VTALERT("Retornando função..")
                    //Finaliza execução
                    Return
                Endif
            Endif
            
        Enddo
        
        @ 0,0 VTSAY "S:"

        VTCLEAR()
    else
        VTALERT("Nenhum registro encontrado com o numero "+cPVenda,"PedidoDVenda Invalido/Vazio",.T.,2000)
        VTCLEAR()

    Endif


Return


/* @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   @@@@@@@@ Funï¿½ï¿½es utilitï¿½rias: @@@@@@@
   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@ */

//////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o TelaSZ8()                                                      //
//Monta tela vtbrowse com os itens do Z8                                //
//Retorna o item escolhido.
/////////////////////////////////////////////////////////////////////////
Static Function TelaSZ8(aZ8Itens)
    Local q
    Private nConte := 01 //contagem de cada item adicionado
    //VTALERT("BREAKPOINT Z8-1")
    //VTALERT("Distribuicao Lote x End","Info",.T.,2000)
    nMaxIncV := len(aZ8Itens)
    aPrep    := Array(nMaxIncV,8)
    aItx := {}
    //VTALERT("BREAKPOINT Z8-2-"+alltrim(str(nMaxIncV)))
    
     
    For q:= 1 to nMaxIncV
        //Transform(aZ8Itens[q,7],"@R 99999999999.9999")
        //VTALERT("BREAKPOINT Z8-3")
        //VTALERT("aZ8Itens5:"+aZ8Itens[q,5])
        aPrep[q] := {strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,2],aZ8Itens[q,3],aZ8Itens[q,4],aZ8Itens[q,5],aZ8Itens[q,6],Transform(aZ8Itens[q,7],"@R 99999999999.9999"),Transform(aZ8Itens[q,8],"@R 99999999999.999"),Transform(aZ8Itens[q,9],"@R 99999999999.999")}
        Aadd(aItx,aPrep[q])
        nConte++
        //VTALERT("BREAKPOINT Z8-4")
    End

    If nMaxIncV > 0
        //VTALERT("BREAKPOINT Z8-4")
        aCab := {"Item    ","Produto","Descricao                ","UM         ","Armz   ","Lote    ","Endereco     ","Qnt    ","Peso Liquido    ","Peso Bruto    "} 
        aSize:= {2,TamSx3("B1_COD")[1],TamSx3("B1_DESC")[1],TamSx3("B1_UM")[1],4,TamSx3("B8_LOTECTL")[1],TamSx3("BF_LOCALIZ")[1],18,18,18}   
        nPos := 1 
        @ 0,0 VTSAY "Itens adicionados:"
        nChoose := VTaBrowse(1,0,7,31,aCab,aItx,aSize)
        VTCLEAR()
    Else
        VTALERT("Menu de itens adicionados vazio. Pressione ENTER para prosseguir.","Itens Adicionados",.T.)
        VTCLEAR()
    Endif


    //Cï¿½digo antigo para validaï¿½ï¿½o com qrcode.
    //Voltei ao codigo antigo pois o loop de validaï¿½ï¿½o serï¿½ feito dentro da propria funï¿½ï¿½o
    // do beepvalid.

/*     Local q
    Private nConte := 01 //contagem de cada item adicionado
    Private bTtfa  := .T.

    VTALERT("Distribuicao Lote x End","Info",.T.,1000)
    nMaxIncV := len(aZ8Itens)
    aPrep    := Array(nMaxIncV,8)
    aItx := {}
    
    // - 

    For q:= 1 to nMaxIncV
        //aPrep[q] := {Item,Produto,Descriï¿½ï¿½o,Um,Armazem,Lote,Endereco,Qnt}

        aPrep[q] := {strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,2],aZ8Itens[q,3],aZ8Itens[q,4],aZ8Itens[q,5],aZ8Itens[q,6],Transform(aZ8Itens[q,7],"@R 99999999999.9999")}
        
        bTst := BeepValid(strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,7],aZ8Itens[q,5])

        While bTtfa
            If bTst == .T.
                //Se o beep for vï¿½lido
                //VTALERT("Bp Valido!","Valido",.T.,0500)
                Aadd(aItx,aPrep[q])
                nConte++
                bTtfa := .F.
            else
                //Caso o beep seja invï¿½lido
                bMynX := MsgYesNo("Dados divergentes. Deseja tentar novamente?","Beep Invalido")
                If bMynX
                    bTst := BeepValid(strZero(nConte,2),aZ8Itens[q,1],aZ8Itens[q,7],aZ8Itens[q,5])
                Else
                    VTALERT("O item nï¿½o serï¿½ adicionado.","Aviso",.T.,3000)
                    bTtfa := .F. 
                    Return
                Endif
            Endif
        Enddo

    End

    If nMaxIncV > 0
        VTCLEAR()
        aCab := {"Item    ","Produto","Descricao                ","UM         ","Armz   ","Lote    ","Endereco     ","Qnt    "} 
        aSize:= {2,TamSx3("B1_COD")[1],TamSx3("B1_DESC")[1],TamSx3("B1_UM")[1],4,TamSx3("B8_LOTECTL")[1],TamSx3("BF_LOCALIZ")[1],14}   
        nPos := 1 
        if len(aItx) == 0
            VTALERT("Nenhum item adicionado/confirmado. A tela nao sera exibida.","Aviso",.T.,3000)
        Else
            @ 0,0 VTSAY "Itens Adicionados:"
            nChoose := VTaBrowse(1,0,7,31,aCab,aItx,aSize)
            VTALERT("Len aZ8:"+str(len(aItx)),"Info")
        Endif

        VTCLEAR()
    Endif */

Return nChoose


////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o LxEMxEst()                                                   //
//Retorna o maximo disponï¿½vel em estoque baseado no lote x endereço.    // 
////////////////////////////////////////////////////////////////////////////

Static Function LxEMxEst(cCodMP)
    Private nMxEst := 0

    IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF

    cQry := "SELECT DISTINCT BF_PRODUTO AS B8_PRODUTO,BF_LOTECTL AS B8_LOTECTL,BF_LOCALIZ,BF_QUANT AS B8_SALDO FROM "+RETSQLNAME("SBF")+" AS SBF WHERE BF_PRODUTO = '"+cCodMP+"' AND BF_QUANT > 0 AND SBF.D_E_L_E_T_ = ' ' AND BF_LOCAL = '01' "
    TCQUERY cQry NEW ALIAS "TMP"
    TCSETFIELD("TMP","B8_DATA","D",8,0)

    DbSelectArea("TMP")
    TMP->(Dbgotop())

    While !TMP->(Eof())
        nMxEst += TMP->B8_SALDO
        TMP->(dbSkip())
    Enddo

    TMP->(dbclosearea())


Return nMxEst


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o LxEDistr()                                                                                         //
//Funcionamento similar ao LxEAdArr().                                                                      //
//ï¿½A funï¿½ï¿½o realiza consulta na query de lote x endereï¿½o com filtragem por lotes mais antigos por primeiro. //
//ï¿½Posteriormente a funï¿½ï¿½o realiza a escolha das linhas  atï¿½ que a quantidade total seja batida.
//ï¿½As linhas podem entrar na Z8 de forma desmembrada. (mesmo produto porï¿½m com as quantidades desmbembradas)



Static Function LxEDistr(cCodMP,nQntprv,cAliasNam,cItemOrig)
    Private cArmz := "01"

    //VTALERT("Conta inc?"+cValToChar(lCntCInc))

    //Puxando Desc e UM de acordo com o código de produto
    If Select("SB1") > 0
        SB1->(DbCloseArea())
    Endif

    DbSelectArea("SB1")
    DbSetOrder(1)
    IF SB1->(DbSeek(xFilial("SB1")+cCodMP))
        //DESC
        If !empty(SB1->B1_DESC)
            cDesc := SB1->B1_DESC
        Else
            cDesc := ""
        Endif

        //Um
        If !empty(SB1->B1_UM)
            cUm := SB1->B1_UM
        Else
            cUm := ""
        Endif

        //Qnt Emb
        If !empty(SB1->B1_QE)
            nQntEmb := SB1->B1_QE
        Else
            nQntEmb := 0
            //VTALERT("Produto sem QntEmb cadastrada.","Aviso",.T.,1000)
        Endif

    Endif


/*     //Separando campos por var
    cArmz    := (cAliasNam)->LOCALX //BF_LOCAL
    cCodMP   := (cAliasNam)->PRODUTO //cCodMP
    cLote    := (cAliasNam)->LOTE //B8_LOTECTL
    cEnd     := (cAliasNam)->LOCALIZ //BF_LOCALIZ
    cSaldoli := (cAliasNam)->SALDO //B8_SALDO */

    //Puxa da tabela temporaria especifica do produto
    //ALIAS: TMP_cCodMP
    //Tabela: puxar por aScan na array aTmpNms
    
    (cAliasNam)->(DbGoTop())

    nCalc := nQntPrv

    //Primeira condição. Se verdadeira faz um loop e pega todos valores da lista. Evita perder tempo nas demais condições
    If nCalc > nMxEstoq
        While !(cAliasNam)->(Eof())
            //para linhas em que o saldo for 0.
            If (cAliasNam)->SALDO == 0
                (cAliasNam)->(DbSkip())
            Else
                //MsgAlert("Beepvalid 2")
                If BeepValid(cCodMP,(cAliasNam)->SALDO,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                    nNumcx := NoRound((cAliasNam)->SALDO / nQntEmb , 0)   //caixas completas
                    nCxInc := Mod((cAliasNam)->SALDO,nQntEmb )           //caixas incompletas

                    nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                    nPesoLli := nTmpPliq * nNumcx //total peso liq
                    
                    //Caixa incompleta:
                    //1- Se existir no calculo;
                    //2- Se habilitado pelo usuario;
                    //* Se as duas condições forem válidas, soma no volume e na quantidade.
                    
                    If nCxInc > 0 .AND. lCntCInc
                        nIncVol := 1 
                        nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb
                        nPesoLli += (nTmpPliq * nCxInc)/nQntEmb

                    Else
                        nIncVol := 0
                    EndIf

                    //* lCntcInc := Se caixa incompleta estiver ativado
                    //* Realizará contagem das caixas incompletas
                    If lCntCInc
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                    Else
                        //* Caso não esteja ativado , zera contagem das caixas incompletas. *
                        //* Solicitação Caio

                        If Empty(nCxInc)
                            nCxInc := 0
                        Endif
                        if lAddProd != .F. //se nao puder adicionar produto.. passado pelo beepvalid()
                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                        endif
                    EndIf

                    nVolTotl := nVolTotl + nNumCx + nIncVol
                    nPlTotl  := nPlTotl+nPesoLli
                    nPbTotl  := nPbTotl+nPesoBli
                    (cAliasNam)->SALDO := 0
                Endif
                (cAliasNam)->(DbSkip())
            Endif
        Enddo
        //encerra execução da função.
        Return
    Endif

    //Caso a primeira condição não seja .T., nCalc recebe nQntPrev e inicia-se a lógica seguinte.
    nCalc := nQntprv

    While nCalc != 0 .AND. !(cAliasNam)->(Eof())
        If  nCalc > (cAliasNam)->SALDO
            //1-Pega o saldo inteiro da linha
		    //2-Subtrai do nCalc 
		    //3- DbSkip()

            if lPular
                Return  
            Endif

            If (cAliasNam)->SALDO == 0
                (cAliasNam)->(DbSkip())
            Else


                If BeepValid(cCodMP,(cAliasNam)->SALDO,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                    nNumcx := NoRound((cAliasNam)->SALDO / nQntEmb,0)   //caixas completas
                    nCxInc := Mod((cAliasNam)->SALDO,nQntEmb)           //caixas incompletas
                    

                    nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                    nPesoLli := nTmpPliq * nNumcx //total peso liq

                    //Caixa incompleta:
                    //1- Se existir no calculo;
                    //2- Se habilitado pelo usuario;
                    //* Se as duas condições forem válidas, soma no volume e na quantidade.
                    
                    If nCxInc > 0 .AND. lCntCInc
                        nIncVol := 1 
                        nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb
                        nPesoLli += (nTmpPliq * nCxInc)/nQntEmb
                    Else
                        nIncVol := 0
                    EndIf

                    // Se Considerar Caixas Incompletas
                    If lCntCInc
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                    Else
                        
                        If Empty(nCxInc)
                            nCxInc := 0
                        Endif

                        if lAddProd != .F. //Se puder adicionar produto.. passado pelo beepvalid()
                            Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,(cAliasNam)->SALDO-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                        endif

                    EndIf 
                    
                    //se nao considerar caixa inc, nao pode subtrair com caixa inc do saldo de controle
                    If !lCntCInc
                        nCalc := nCalc  - (cAliasNam)->SALDO + nCxInc
                    else
                        nCalc := nCalc + (cAliasNam)->SALDO
                    Endif


                    nVolTotl := nVolTotl + nNumCx + nIncVol
                    nPlTotl  := nPlTotl+nPesoLli
                    nPbTotl  := nPbTotl+nPesoBli
                    (cAliasNam)->SALDO := 0
                
                    (cAliasNam)->(DbSkip())
                Endif
            Endif

        Elseif nCalc <= (cAliasNam)->SALDO

            //Adiciona o valor total do nCalc e encerra execução.
            //VTALERT("beepvalid 4 ")
            If BeepValid(cCodMP,nCalc,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nQntEmb,lCntCInc)
                nNumcx := NoRound(nCalc / nQntEmb , 0)   //caixas completas
                nCxInc := Mod(nCalc,nQntEmb )           //caixas incompletas

                nPesoBli := nTmpPbrt * nNumcx //total peso bruto
                nPesoLli := nTmpPliq * nNumcx //total peso liq

                //Caixa incompleta:
                //1- Se existir no calculo;
                //2- Se habilitado pelo usuario;
                //* Se as duas condições forem válidas, soma no volume e na quantidade.
                
                If nCxInc > 0 .AND. lCntCInc
                    nIncVol := 1 
                    nPesoBli += (nTmpPbrt * nCxInc)/nQntEmb //regra de tres puxando peso bruto de acordo com qnt caixa inc
                    nPesoLli += (nTmpPliq * nCxInc)/nQntEmb
                Else
                    nIncVol := 0
                EndIf
                
                //Se considerar caixas incompletas.
                If lCntCInc
                    Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nCalc,nTmpPliq,nTmpPbrt,nNumCx,nCxInc,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                Else

                    If Empty(nCxInc)
                        nCxInc := 0
                    Endif

                    if lAddProd != .F. //Se puder adicionar produto.. passado pelo beepvalid()
                        Aadd(aZ8Itens,{cCodMp,cDesc,cUm,cArmz,(cAliasNam)->LOTE,(cAliasNam)->LOCALIZ,nCalc-nCxInc,nTmpPliq,nTmpPbrt,nNumCx,0.000,cItemOrig,dToC(StoD(aIncVB[nChoose,4])),lCntCInc})
                    endif
                Endif
                

                nVolTotl           := nVolTotl + nNumCx + nIncVol
                nPlTotl            := nPlTotl+nPesoLli
                nPbTotl            := nPbTotl+nPesoBli
                nSobra             := (cAliasNam)->SALDO - nCalc
                nCalc              := nCalc - nCalc
                (cAliasNam)->SALDO := nSobra
                
                //ImpZRsto(cCodMP,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,nSobra,nTmpPliq) //Imprime Sobra Etiqueta Zebra.
                VTCLEAR()
            Endif
            Return
        Endif
    Enddo

Return


////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o BeepQr(aItens)                                      //
//Beep para soma do volume,caixa,peso bruto e peso lï¿½quido  //
/////////////////////////////////////////////////////////////

Static Function BeepQr(aItens)  
    Local q
    Private aIty        := {}
    Private nConte      := 01
    Private bLpBeep     := .T.
    Private cBpqr       := SPACE(45) //String principal beep qrcode
    Private cQtCx       := SPACE(10)
    Private cQtCxInc    := SPACE(10)
    Private cQtPrcx     := SPACE(10)
    Private cPbruto     := SPACE(10)
    Private cPliquido   := SPACE(10)
    Private cDelimit    := "$"

    VTCLEAR()        
    @ 0,0 VTSAY "Item - "+StrZero(nChoose,2)
    @ 1,0 VTSAY "QrCode"
    @ 2,0 VTSAY "QR:" VTGET cBpQr VALID MxBpQnt(cBpQr,cChoose,aIty) == .T.
    VTREAD
    VTCLEAR()

    //Ordem das informações do QrCode:
    //Produto, Qnt,Lote, Peso Líquido, Peso Bruto

    //Separar Dados QrCode
    nSearch := AT(cDelimit,cBpQr)
    //Produto
    nAx      := AT(cDelimit,cBpQr,1)
    cQcprod  := Substring(cBpQr,0,nAx-1) //pegando do primeiro ate o ultimo simbolo
    cMainStr := Substring(cQxprod,,nAx+1,len(cQcprod))
    cProd := PADR(cQcprod,15)
    //Qnt
    nQx      := AT(cDelimit,cMainStr,1)
    cQnt     := Substring(cMainStr,0,nQx-1)
    cMainStr := Substring(cMainStr,nQx+1,len(cMainStr))
    nQtde    := cQnt

    //Lote
    nLx      := AT(cDelimit,cMainStr,1)
    cQLote   := Substring(cMainStr,0,nLx-1)
    cMainStr := Substring(cMainStr,nLx+1,len(cMainStr))
    cLote    := PADR(cQLote,10)

    //Peso Liquido
    nMx      := AT(cDelimit,cMainStr)
    cQPesol  := Substring(cMainStr,0,nLx-1)
    cMainStr := Substring(cMainStr,nMx+1,len(cMainStr))
    cPesol   := PADR(cQPesol,10)

    //Peso Bruto
    nZx      := AT(cDelimit,cMainStr)
    cQPesob  := Substring(cMainStr,0,nZx-1)

Return


//@@@@@@@@@@@@@@@@@@@@@@@@
//@Funï¿½ï¿½es de validaï¿½ï¿½o:@@
//@@@@@@@@@@@@@@@@@@@@@@@@

//Pedido de venda
Static Function ValPVenda(cPVenda)
    DbSelectArea("SC6")
    DbSetOrder(1)
    If SC6->(DbSeek(xFilial("SC6")+cPVenda))
        Return .T.
    else
        Return .F.
    Endif

Return

////////////////////////////////////////////////////////////////////////
//Quantidade BeepQr                                                   //
//A quantidade beepada nao pode ser superior a quantidade da linha    //
////////////////////////////////////////////////////////////////////////

Static Function MxBpQnt(cBpQr,cChoose,aIty) 
    
    Local bMxBpQnt
    

    If !empty(cBpQr)
        nLQnt := aIty[nChoose,8]

        //cTstX := Valtype(cBpQr)   C
        //cTstY := Valtype(cLQnt)   N
        //VTALERT("cTstX:"+cTstX+"-cTsty:"+cTstY)

/*         If val(cBpQr) <= cLQnt
            //Tudo ok, quantidade menor ou igual.
            bMxBpQnt := .T.
        Else
            VTALERT("Quantidade beepada excede o total do item "+cChoose+".","Erro Qnt",.T.,3000)
            bMxBpQnt := .F.    
        Endif */

        nRes := val(cBpQr) - cLQnt
        //VTALERT("nLQnt:"+nLQnt)
        If nRes != 0 //se o resto nao for zero, significa que as quants sao diferentes.
            //VTALERT("A quantidade beepada "+cBpQr+" e "+str(cLQnt)+" sao divergentes.")
            @ 0,0 VTSAY "A quantidade beepada ("+alltrim(cBpQr)+") e QntLinha ("+TRANSFORM(nLQnt,"@R 99999999999.9999")+") sao divergentes."
            @ 1,0 VTSAY "Deseja continuar?"
            nOp:=VTaChoice(2,0,6,VTMaxCol(),{"Sim","Nao"})
            If nOp == 1
                bMxBpQnt := .T.
                Return
            elseif nOp == 2
                bMxBpQnt := .F.
                Return
            Endif 
        else
            bMxBpQnt := .T.
        Endif
    Else
        bMxBpQnt := .F.    
    Endif

Return bMxBpQnt

//////////////////////////////////////////////////////////////////////////////////////////////////
//Funï¿½ï¿½o para confirmaï¿½ï¿½o do Prod/Lote.                                                         //
//Retorna .T. caso o produto/lote seja o mesmo do Qr beepado - Retorna .F. caso nï¿½o seja.       // 
//////////////////////////////////////////////////////////////////////////////////////////////////

Static Function BeepValid(cZProd,cZqnt,cZlote,cZLocal,cZQntEmb,lCntCInc)
   
    Private bIsBpVld := .F. //booleano final. Caso .F., o item nï¿½o ï¿½ adicionado a array final e o programa entra em loop.
    Private cBpQr    := SPACE(45)
    Private cDelimit := "$"
    Private bValProd := .F.
    Private bValQnt  := .F.
    Private bValLote := .F.
    Private bIsEdVld := .F.
    Private bLoop    := .T.
    Private cEndCnf  := SPACE(TamSx3('BF_LOCALIZ')[1])
    Private nZvol    := 0
    Private nCxInz   := 0
    Private nIncVolz := 0
    Private nZvoltl  := 0
    Private nVoltt   := 0 //Volume total
    Public nTmpPliq  := 0 //Peso Lï¿½quido Temporï¿½rio. Para inclusao na array da lista final
    Public nTmpPbrt  := 0

    lAddProd := .T. //reset addprod
    //Calculo volumes
    nZvol  := NoRound(cZqnt / cZQntEmb , 0)   //caixas completas
    nCxInz  := Mod(cZqnt,cZQntEmb )           //caixas incompletas

    //Para casos em que a opção "Considerar Caixa Incompleta" estiver desativado
    //e houver somente caixas incompletas.
    //VTALERT("cZqnt:"+str(cZqnt)+"-cZQntEmb:"+str(cZQntEmb)+"-lCntciNC"+cValToChar(lCntCInc))

    //caso a quantidade a beepar seja somente caixa incompleta e a opcao "consid caixa inc?" for "não".
    If  cZqnt-nCxInz <= 0.0000 .AND. nCxInz != 0.000 .AND. !lCntCInc
        lAddProd := .F.
        Return bIsBpVld := .T.    
    Else


        //Considera tambem caixas incompletas como vol + 1
        If nCxInz > 0 .AND. lCntCInc
            nIncVolz += 1
        Else
            //caso contrario passa qnt vol caixa inc zerada
            nIncVolz := 0
        Endif

        nZvoltl := nZvol +nIncVolz

        While bLoop
            cError := ""
            cZxprod := padr(cZProd,TamSx3("B1_COD")[1])
            cZxlote := padr(cZxlote,TamSx3("B8_LOTECTL")[1])

            VTCLEAR()
            @ 0,0 VTSAY "-Confirme o Item-"
            @ 1,0 VTSAY "Prod - "+cZProd

            If lCntCInc
                @ 2,0 VTSAY "Qnt  - "+alltrim(Transform(cZqnt,"@R 99999999999.9999"))
            Else
                @ 2,0 VTSAY "Qnt  - "+alltrim(Transform(cZqnt-nCxInz,"@R 99999999999.9999"))        
            Endif


            @ 3,0 VTSAY "Lote - "+cZLote
            @ 4,0 VTSAY "End  - "+cZLocal
            @ 5,0 VTSAY "Vol  - "+alltrim(str(nZvoltl))
            @ 6,0 VTSAY "---------------------------"
            @ 7,0 VTSAY "QR:" VTGET cBpQr
            VTREAD

            cQxProd := cBpQr

            //nSearch = >0 para qrcode / 0 para nï¿½o QrCode.
            nSearch := AT(cDelimit,cQxProd)
            
            If nSearch != 0
                
                //Separação de Informações do QrCode
                //Produto
                nAx      := AT(cDelimit,cQxProd,1)
                cQcprod  := Substring(cQxProd,0,nAx-1) //pegando do primeiro ate o ultimo simbolo
                cMainStr := Substring(cQxProd,nAx+1,len(cQxProd))
                cProd    := PADR(cQcprod,TamSx3("B1_COD")[1])

                //Qnt
                nQx      := AT(cDelimit,cMainStr,1)
                cQnt     := Substring(cMainStr,0,nQx-1)
                cMainStr := Substring(cMainStr,nQx+1,len(cMainStr))
                nQtde    := cQnt
                
                //Lote
                nLx      := AT(cDelimit,cMainStr,1)
                cQLote   := Substring(cMainStr,0,nLx-1)
                cMainStr := Substring(cMainStr,nLx+1,len(cMainStr))
                cLote    := PADR(cQLote,TamSx3("B8_LOTECTL")[1])

                //Peso Líquido
                nYx      := AT(cDelimit,cMainStr,1)
                cQPesol  := Substring(cMainStr,0,nYx-1)
                cMainStr := Substring(cMainStr,nYx+1,len(cMainStr))
                cPesol   := PADR(cQPesol,TamSx3("B1_PESO")[1])

                //Peso Bruto
                nZx      := AT(cDelimit,cMainStr,1)
                cQPesob  := Substring(cMainStr,0,nZx-1)
                cMainStr := Substring(cMainStr,nZx+1,len(cMainStr))
                cPesob   := PADR(cQPesob,TamSx3("B1_PESO")[1])

                //Utilização do StrTran para corrigir bug nas casas decimais peso liquido e peso bruto.
                //Solução feita no ACD para evitar alteração da etiqueta zebra de novo.    
                //StrTran( < cString >, < cSearch >, [ cReplace ], [ nStart ], [ nCount ] )   

                cSearch  := "," //caracter a pesquisar
                cReplace := "." //caracter a substituir
                cPesol := StrTran(cPesol,cSearch,cReplace) //troca , por . no Peso Liquido.
                cPesob := StrTran(cPesob,cSearch,cReplace) //troca , por . no Peso Bruto.
                
                VTALERT("Produto:"+cProd+"-Qnt:"+nQtde+"-Lote:"+cLote+"-Peso Liq:"+cPesol+"-Peso Bruto:"+cPesob,"Info QrCode",.T.,4000)
                VTCLEAR()

                
                //Pular ou nao a tela do beep endereço.
                lPula := .F.

                //Caso der erro, já vai para tela do erro antes de rodar o beep endereço.
                If alltrim(cProd) != alltrim(cZProd) .OR. alltrim(cLote) != alltrim(cZLote)
                    //VTALERT("Produto ou Lote Invalido.","Erro Beep",.T.,3000)
                    lPula := .T.
                Else
                    lPula := .F.
                EndIf

                If !lPula
                    @ 0,0 VTSAY "-Confirme o Endereco-"
                    @ 1,0 VTSAY "End  - "+cZLocal
                    @ 2,0 VTSAY "---------------------------"
                    @ 3,0 VTSAY "Beep:" VTGET cEndCnf
                    VTREAD
                    
                    //Checa se o endereço é valido. Caso nao seja, se faz necessário beepar
                    //tudo novamente.
                    If alltrim(cEndCnf) == alltrim(cZLocal)
                        VTALERT("Endereço confirmado:"+alltrim(cZLocal)+".","Ok",.T.,1000)
                        bIsEdVld  := .T.
                        bIsBpVld  := .T.
                    Else
                        //VTBEEP(2)
                        VTALERT("Erro ao confirmar o endereço. Beep invalido.","Erro",.T.,2000)
                        bIsEdVld  := .F.
                        bIsBpVld  := .F.
                    Endif

                Endif
            
                //Compara dados do QrCode com os dados 
                //Caso houver algum dado divergente, volta para tela do adicionar mais itens.
                If alltrim(cProd) == alltrim(cZProd) .AND. alltrim(cLote) == alltrim(cZLote) .AND. bIsEdVld
                    //confirmaï¿½ï¿½o prod e lote
                    VTBEEP(1)
                    nTmpPliq  := val(cPesol)+nTmpPliq //Somatoria Peso Liquido.. Só soma se confirmar prod+lote.
                    nTmppbrt  := val(cPesob)+nTmpPbrt //Somatória Peso Bruto.. Só soma se confirmar prod+lot.
                    bLoop    := .F.
                    VTCLEAR()
                Else
                    //--Verifica qual das informaï¿½ï¿½es estï¿½ divergente--
                    VTBEEP(2)
                    
                    //Valida Produto
                    If alltrim(cProd) != alltrim(cZProd)
                        //VTALERT("Beep invalido. Erro Produto.","Beep Invalido",.T.,2000)
                        cError += "Produto,"
                    Endif
                    
                    //Valida Lote
                    If alltrim(cLote) != alltrim(cZLote)
                        //VTALERT("Beep Invalido. Erro Lote.","Beep Invalido",.T.,2000)
                        cError += "Lote,"
                    Endif

                    //Se existir algum erro.
                    If len(cError) > 1
                        VTALERT("Beep invalido. Erro:"+cError,"Beep Invalido.",.T.,3000)
                    Endif

                    VTCLEAR()
                    @ 0,0 VTSAY "Dados divergentes."
                    @ 1,0 VTSAY "Tentar novamente?"
                    nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                    If nOp == 1
                        //reset string qr
                        cBpQr   := SPACE(45)
                        cEndCnf := SPACE(TamSx3('BF_LOCALIZ')[1])
                        VTALERT("Aguarde..","Carregando",.T.,500)
                        lPular := .T.
                    
                    Else
                        VTALERT("A linha nao sera adicionada.","Aviso",.T.,3000)
                        VTCLEAR()


                        //***TIRA OS ITENS QUE NAO FORAM ADICIONADOS DA ARRAY DE TRAVA*****
                        nPosIc := aScan(aReadyad,cItemAt)
                        //Deleta item que nao foi adicionado da array
                        aDel(aReadyAd,nPosIc)
                        //Redimensiona o Array
                        aSize(aReadyAd, Len(aReadyAd)-1)


                        lPular := .T.

                        Return bIsBpVld := .F.
                    Endif

                EndIf

            Else
                VTBEEP(2)
                VTCLEAR()
                @ 0,0 VTSAY "QR invalido."
                @ 1,0 VTSAY "Tentar novamente?"
                nOp:=VTaChoice(3,0,6,VTMaxCol(),{"Sim","Nao"})
                If nOp == 1
                    VTCLEAR()
                    cBpQr := SPACE(45)
                    cEndCnf := SPACE(TamSx3('BF_LOCALIZ')[1])
                    VTALERT("Carregando..","Aguarde",.T.,500)
                Else
                    VTALERT("A linha nao sera adicionada.","Aviso",.T.,3000)
                    VTCLEAR()

                    //***TIRA OS ITENS QUE NAO FORAM ADICIONADOS DA ARRAY DE TRAVA*****
                    nPosIc := aScan(aReadyad,cItemAt)
                    //Deleta item que nao foi adicionado da array
                    aDel(aReadyAd,nPosIc)
                    //Redimensiona o Array
                    aSize(aReadyAd, Len(aReadyAd)-1)

                    lPular := .T.

                    Return bIsBpVld := .F.

                Endif

            Endif
        Enddo

    Endif

Return bIsBpVld




/////////////////////////////////////////
//Função para Envio de Email Automatico//
////////////////////////////////////////

Static Function AutoEml(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)
    Local cAcnt		:= ""
	Local cServer	:= ""
	Local cPsw		:= ""
    Private cCodC := ""

    DbSelectArea("SC5")
    SC5->(DbSetOrder(1))
    If DbSeek(xFilial("SC5")+cPddv)
        cCodC := SC5->C5_CLIENTE
    Endif
    SC5->(DbCloseArea())

    DbSelectArea("SZ7")
    SZ7->(DbSetOrder(1))
    If DbSeek(XFilial("SZ7")+cNumPL)
        //Qnt Volume
        If !empty(SZ7->Z7_QVOL)
            nQntVol := SZ7->Z7_QVOL
        Else
            nQntVol := 0.0000
        Endif
        //Total Peso Lï¿½quido
        If !empty(SZ7->Z7_PESOL)
            nPesL := SZ7->Z7_PESOL
        Else
            nPesL := 0.0000    
        Endif
    Endif

	cAcnt		:= Alltrim(GETMV("MV_EMCONTA"))
	cServer	    := Alltrim(GETMV("MV_RELSERV"))
	cPsw		:= Alltrim(GETMV("MV_EMSENHA"))
    cMlTo       := Alltrim(GETMV("MV_PLSMLTO"))

	cHtml := ObtemHtm(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)
	lResulSend := .T.
	lEnvio := .T.

	CONNECT SMTP SERVER Alltrim(cServer) ACCOUNT Alltrim(cAcnt) PASSWORD Alltrim(cPsw) RESULT lEnvio 
	If !lEnvio 
		 MsgStop("Erro na emissao do E-mail (Conexao) !!!") 
	Else
		//cTo := "Arinus@NewCompton.com.br ;"
		//cTo := "almoxarifado@msadobrasil.com.br ; Arinus@NewCompton.com.br ; fiscal@msadobrasil.com.br ; fiscal2@msadobrasil.com.br ; logistica@msadobrasil.com.br ; caio@msadobrasil.com.br ; almoxarifado2@msadobrasil.com.br;contabilidade@msadobrasil.com.br;"
        
        cTo := cMlTo


        cCC := ""
		MailAuth(Alltrim(cAcnt),Alltrim(cPsw))
		SEND MAIL FROM Alltrim(cAcnt) TO cTo CC cCC SUBJECT "[ACD]Notificação Automática - Picklist de Saída N-"+alltrim(cNumPl) BODY cHtml RESULT lResulSend
	Endif

	If !(lResulSend) 
		_cMailError := ""
		Get Mail Error _cMailError 
		Alert("Não foi possível enviar o email. Erro: "+ _cMailError) 
	EndIf 

	DISCONNECT SMTP SERVER

Return             


Static Function ObtemHtm(cNumPl,cData,cHora,cUsuario,cPddv,aZ8Insrt,cStatus)

	Local   cRet := ""
    Local   W,x
    Private nMaxIt := len(aZ8Insrt) //Total de itens
    Private nBQe //Qnt por embalagem
    Private nNumCx := 0
    Private nCxInc := 0

	//Fixo
	cRet += "<!DOCTYPE html>"
	cRet += "<html>"
	cRet += "<head>"
	cRet += "<style>"
	cRet += "table, th, td {"
	cRet += "border: 1px solid black;"
	cRet += "border-collapse: collapse; 
	cRet += "}"
	cRet += "</style>"
	cRet += "</head>"
	cRet += "<body>"
	cRet += "<h1> Notificação Automática - Picklist de Saída </h1>"
	//cRet += "<h3> Cabeçalho:</h3>"	
	cRet += "<table>"
	cRet += "<tr>"
    cRet += "<th> Cod. Cliente </th>"
	cRet += "<th> Num Picklist </th>"
	cRet += "<th> Data </th>"
	cRet += "<th> Hora </th>"
	cRet += "<th> Solicitante </th>"
	cRet += "<th> Num PdV </th>"
    cRet += "<th> Status </th>"
    cRet += "<th> Natureza Operação </th>"
	cRet += "</tr>"

	//Dinamico - Cabeçalho Picklist
	cRet += "<tr>"
    cRet += "<td> "+PADR(cCodC,TamSx3('C5_CLIENTE')[1])+" </td>"
	cRet += "<td> "+PADR(cNumPl,TamSx3("Z7_NUM")[1])+" </td>"
	cRet += "<td> "+cData+" </td>"
	cRet += "<td> "+cHora+" </td>"
	cRet += "<td> "+PADR(cUsuario,TamSx3("Z7_SOLIC")[1])+" </td>"
	cRet += "<td> "+PADR(cPddv,TamSx3("Z7_PEDIDO")[1])+" </td>"

    //Status Picklist
    If cStatus == "A"
        cRet += "<td> Aberto </td>"
    Elseif cStatus == "F"
        cRet += "<td> Faturado </td>"
    Endif
    

    //Natureza Operação
    If cNatOp == "V"
        cRet += "<td> Venda </td>"
    Elseif cNatOp == "B"
        cRet += "<td> Beneficiamento </td>"
    Elseif cNatOp == "D"
        cRet += "<td> Devolução </td>"
    Elseif cNatOp == "R"
        cRet += "<td> Retrabalho </td>"
    Elseif cNatOp == "O"
        cRet += "<td> Outras Saídas </td>"
    Endif
    
	cRet += "</tr>"
	cRet += "</table>"

    //Dinamico - Itens Picklist
    cRet += "<h3>Itens:</h3>"
    cRet += "<table>"
    cRet += "<tr>"
    cRet += "<th>Item</th>"
    cRet += "<th>Código Produto</th>"
    cRet += "<th>Descrição</th>"
    cRet += "<th>U.M</th>"
    cRet += "<th>Armazém</th>"
    cRet += "<th>Localização</th>"
    cRet += "<th>Lote</th>"
    cRet += "<th>Quantidade</th>"
    cRet += "<th>Peso Líquido</th>"
    cRet += "<th>Peso Bruto</th>"
    cRet += "<th>Caixas Completas</th>"
    cRet += "<th>Caixa Incompleta (Qnt)</th>"
    cRet += "<th>Data Entrega</th>"
    cRet += "</tr>"
    
    //cCodMp,cDesc,cUm,cArmz,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,TMP->B8_SALDO,nTmpPliq,nTmpPbrt,nNumCx,nCxInc
    For W := 1 To nMaxIt

        cRet += "<tr>"
        cRet += "<td>"+strzero(W,2)+"</td>" //item
        cRet += "<td>"+aZ8Insrt[w,1]+"</td>" //Código Produto
        cRet += "<td>"+aZ8Insrt[w,2]+"</td>" //Descrição
        cRet += "<td>"+aZ8Insrt[w,3]+"</td>" //U.M
        cRet += "<td>"+aZ8Insrt[w,4]+"</td>" //Armazém
        cRet += "<td>"+aZ8Insrt[w,6]+"</td>" //Lote
        cRet += "<td>"+aZ8Insrt[w,5]+"</td>" //end
        cRet += "<td><font color='blue'><b>"+Transform(aZ8Insrt[w,7],"@R 99999999999.999")+"</b></font></td>" //saldo
        cRet += "<td>"+Transform(aZ8Insrt[w,8],"@R 99999999999.999")+"</td>" //peso liquido
        cRet += "<td>"+Transform(aZ8Insrt[w,9],"@R 99999999999.999")+"</th>" //peso bruto
        cRet += "<td>"+alltrim(str(aZ8Insrt[w,10]))+"</td>" //caixa comp
        cRet += "<td>"+Transform(aZ8Insrt[w,11],"@R 99999999999.999")+"</th>" //caixa inc
        cRet += "<td>"+aZ8Insrt[w,13]+"</td>"
        cRet += "</tr>"
    End

    cRet += "</table>"
    cRet += "<h3>Resumo de Carga:</h3>"
    cRet += "<table>"
    cRet += "<tr>"
    cRet += "<th>Total Volume</th>"
    cRet += "<th>Total Peso Líquido</th>"
    cRet += "<th>Total Peso Bruto </th>"
    cRet += "</tr>"
    cRet += "<tr>"
    cRet += "<td>"+cValToChar(nVolTotl)+"</td>"
    cRet += "<td>"+Transform(nPlTotl,"@R 99999999999.999")+"</td>"
    cRet += "<td>"+Transform(nPbTotl,"@R 99999999999.999")+"</td>"
    cRet += "</tr>"
    cRet += "</table>"
    cRet += "</body>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<br>"
    cRet += "<footer><h9><b>***E-mail Automático - Sistema Protheus - Não responder***</b></h9></footer>"

Return cRet


////////////////////////////////////////////
//Checa itens duplicados                  //
// Retorna .T. para dupl e .F. para n dupl//
////////////////////////////////////////////

Static Function CheckDup(cItem,aReadyad)
    Local nPos
    Private bCheck

    nPos := aScan(aReadyad,cItem)

    If nPos == 0
        bCheck := .F.
    Else
        bCheck := .T.   
    Endif

Return bCheck

///////////////////////////////////////////////////////////////////
//Função de Impressão de Etiqueta A4 de Estoque                 //
//Imprime Etiqueta A4 de Estoque de saldo Remanescente Spool   //
//@Parametros: Produto,Lote,Endereço,Saldo.                   //
///////////////////////////////////////////////////////////////

Static Function ImpZRsto(cProd,cLote,cLocaliz,nSobra,nTmpPliq,cCliMan)
    //////////////////////////////////////
    //Parametro de Escolha Impressora.  //
    //MV_IMPCHS1 = Impressora 1           //
    //MV_IMPCHS2 = Impressora 2           //
    //////////////////////////////////////
    Local X
    local cRelName as char
    local lAdjust as logical
    local nPrintType as numeric
    local oFont10 as object
    local oPrinter as object
    local cText as char
    local nSize as numeric
    Private cCliMan  := SPACE(15) 
    Private cDescrip := ""
    Private aTst     := {}
    //PUTMV("MV_IMPCHS1","")
    //PUTMV("MV_IMPCHS2","") 
    
    cImpOne := GetMV("MV_IMPCHS1") //Pega o endereço da impressora 1.
    cImpTwo := GetMV("MV_IMPCHS2") //Pega o endereço da impressora 2.

    //VTALERT("Imp1:"+alltrim(cImpOne)+"-Imp2:"+alltrim(cImpTwo))

    VTCLEAR()

    //Como será a4 de receb, não é necessário.
/*     @ 0,0 VTSAY "Informacoes:"
    @ 1,0 VTSAY "Cliente:" VTGET cCliMan
    VTREAD */

    VTCLEAR()
    @ 0,0 VTSAY "Selecione a impressora:"
    nOp:=VTaChoice(1,0,6,VTMaxCol(),{"Logistica","Estoque"})

    If nOp == 1
        //Impressora 1
        cPrinter := "RICOH MP C3003"

    Elseif nOp == 2
        //Impressora 2
        cPrinter := GetMV("MV_IMPCHS2")
    Endif

    cCodmsa  := alltrim(cProd) //Codigo Msa (Produto)
        
    //descrição
    DbSelectArea("SB1")
    SB1->(DbSetOrder(1))
    If SB1->(DbSeek(xFilial()+mv_par02))

        //Puxando Descrição Do Produto
        If !Empty(SB1->B1_DESC)
            cDescrip :=  SB1->B1_DESC
        else
            cDescrip := ""
        Endif

        //Puxando Código Cliente
        If !Empty(SB1->B1_XXCODCL)
            cCodCli := SB1->B1_XXCODCL
        Else
            cCodCli := ""
        Endif

    Endif

    cQntde   := Transform(nSobra,"@E 999,999.999")
    cLote    := alltrim(cLote) // Lote
    cLmatp   := "" // Lote Materia Prima
    cDatemb  := "99/99/99" // Data da Embalagem
    cPesobrt := "1 KG"

    cPesoliq := Transform(nTmpPliq,"@E 999,999.999")

    cCliente := alltrim(cCliMan)

    cRelName   := "EtqEstoque"+alltrim(str(Random(0,999)))
    lAdjust    := .F.
    nPrintType := 2  //IMP_SPOOL

    oFont05    := TFont():New("ARIAL",05,05,,.F.,,,,,.F.,.F.) ///Fonte 5 Normal
    oFont07    := TFont():New("ARIAL",07,07,,.F.,,,,,.F.,.F.) ///Fonte 7 Normal
    oFont10    := TFont():New("ARIAL",10,10,,.F.,,,,,.F.,.F.) ///Fonte 10 Normal
    oFont15    := TFont():New("ARIAL",15,15,,.F.,,,,,.F.,.F.) ///Fonte 15 Normal
    oFont17    := TFont():New("ARIAL",17,17,,.F.,,,,,.F.,.F.) ///Fonte 17 Normal
    oFont20    := TFont():New("ARIAL",20,20,,.F.,,,,,.F.,.F.) ///Fonte 20 Normal
    oFont25    := TFont():New("ARIAL",25,25,,.F.,,,,,.F.,.F.) ///Fonte 25 Normal
    oFont30    := TFont():New("ARIAL",30,30,,.F.,,,,,.F.,.F.) ///Fonte 30 Normal
    oFont35    := TFont():New("ARIAL",35,35,,.F.,,,,,.F.,.F.) ///Fonte 35 Normal

    oFont25b   := TFont():New("ARIAL",25,25,,.T.,,,,,.F.,.F.) ///Fonte 25 Normal

    oPrinter := FWMSPrinter():New(cRelName, nPrintType , lAdjust, "/SPOOL/", .T., /*lTReport*/ ,,cPrinter, .F. ,)
    //oPrint := FwMsPrinter():New('NOME RELATORIO', IMP_SPOOL, .T.,,.T.,,,'NOME_IMPRESSORA')
    oPrinter:SetLandscape()
    oPrinter:SetPaperSize(DMPAPER_A4)
    oPrinter:setCopies(1)
    
    
    nCenterPg := Round(oPrinter:nHorzSize() / 2 ,0)

    oPrinter:StartPage() // Inicia uma nova pagina
    oPrinter:SetParm( "-RFS")

    //Construção do Layout - Boxes.
    //FWMsPrinter(): Box ( < nRow>, < nCol>, < nBottom>, < nRight>, [ cPixel] ) -->
    oPrinter:Box( 15, 5, 600, 836, "-2")
    oPrinter:Box( 15, 5, 200, 200, "-2")
    oPrinter:Box( 200, 5, 400, 200, "-2")
    oPrinter:Box( 400, 5, 581, 200, "-2")

    //Construção do Layout - Linhas
    //FWMsPrinter(): Line ( < nTop>, < nLeft>, < nBottom>, < nRight>, [ nColor], [ cPixel] ) -->

    oPrinter:Line( 107, 200, 107, 836)
    oPrinter:Line( 200, 200, 200, 836)
    oPrinter:Line( 285, 200, 285, 836)
    oPrinter:Line( 370, 200, 370, 836)
    oPrinter:Line( 400, 200, 400, 836)

    //rodapé
    oPrinter:Line( 581, 5, 581, 836)

    oPrinter:SayBitmap( 27,27 , "/SYSTEM/LGMID.PNG",150,150)
    
    //Fixos - Say
    oPrinter:Say(210, 85, "Qr Code",oFont10)
    oPrinter:Say(410, 70, "Esboço de Peça",oFont10)
    oPrinter:Say(595,260,"M.S.A do Brasil Ltda - São José dos Campos - SP - www.msadobrasil.com.br",oFont10)
    oPrinter:Say(30,450,"A4 Estoque",oFont10)
    
    //Fixo + Var+Codebar
    oPrinter:Say(66,215,"Código Cliente",oFont25)
    oPrinter:Say(85,215,"(Part Number Customer)",oFont15)
    oPrinter:Say(66,395,cCodCli,oFont20)
    oPrinter:Code128(73, 396,cCodCli, 1, 27)
    
    oPrinter:Say(155,215,"Código M.S.A",oFont25)
    oPrinter:Say(174,215,"(Part Number M.S.A)",oFont15)
    oPrinter:Say(155,395,cCodMsa,oFont20)
    oPrinter:Code128(162,396,cCodMsa, 1, 27)

    oPrinter:Say(240,215,"Descrição",oFont25)
    oPrinter:Say(255,215,"(Description)",oFont15)
    oPrinter:Say(240,395,cDescrip,oFont20)

    oPrinter:Say(330,215,"Quantidade",oFont25)
    oPrinter:Say(349,215,"(Quantity)",oFont15)
    //oPrinter:Say(330,395,alltrim(Transform(cQntde,"@E 999,999.999")),oFont20)
    //oPrinter:Code128(337,396,alltrim(Transform(cQntde,"@E 999,999.999")), 1, 27)


    oPrinter:Say(390,215,"Cliente",oFont15)
    oPrinter:Say(398,215,"(Customer)",oFont07)
    oPrinter:Say(390,395,cCliente,oFont15)

    oPrinter:Say(420,215,"Lote",oFont17)
    oPrinter:Say(430,215,"(Lot)",oFont07)
    oPrinter:Say(420,395,cLote,oFont17)
    oPrinter:Code128(405,595,cLote, 1, 27)

    oPrinter:Say(455,215,"Lote Materia Prima",oFont17)
    oPrinter:Say(465,215,"(Raw Material Lot)",oFont07)
    oPrinter:Say(455,395,cLmatp,oFont17)
 
    oPrinter:Say(490,215,"Data da Embalagem",oFont17)
    oPrinter:Say(500,215,"(Date of Packaging)",oFont07)
    oPrinter:Say(490,395,"",oFont17)

    oPrinter:Say(525,215,"Peso Bruto",oFont17)
    oPrinter:Say(535,215,"(Gross Weight)",oFont07)
    oPrinter:Say(525,395,alltrim(transform(cPesobrt, "@E 999,999.99")),oFont17)

    //alltrim(transform(mv_par03,"@E 999,999.999"))
    oPrinter:Say(560,215,"Peso Líquido",oFont17)
    oPrinter:Say(570,215,"(Net Weight)",oFont07) 
    //nPEst := mv_par08 * mv_par03
    oPrinter:Say(560,395,alltrim(transform(mv_par10,"@E 999,999.99")) ,oFont17)

    //QrCode
    //cQCodMsa := alltrim(cCodMsa)+"$"
    //cQCodCli := alltrim(cCodCli)+"$"
    //cQQnt 	:= alltrim(str(mv_par03))+"$"
    //cQLote  := alltrim(cLote)+"$"
    //cQPbrt := alltrim(cPesobrt)+"$"
    //cQPliq := alltrim(cPesoliq)+"$"
    //cQrstr  := cQCodMsa+cQQnt+cQLote+cQPbrt //string qrcode full

    //QrCode
    //oPrinter:QRCode(375,27,cQrstr, 160)

/*     If bPecaA4
        if cImgTpya == "PNG"
            oPrinter:SayBitmap( 433,57 , "/SYSTEM/"+alltrim(mv_par02)+".png",,)
        Elseif cImgTpya == "JPG"
            oPrinter:SayBitmap( 433,57 , "/SYSTEM/"+alltrim(mv_par02)+".jpg",,)

        Endif
        
    Endif */

    oPrinter:EndPage()
    //oPrinter:Preview() - Para pdf preview, Para impressão direto impressora Print
    oPrinter:Print()

    

Return



////////////////////////////////////////////////////////////////////////////////////
// @Função: LxeTmp(cCodMP)                                                        //
// @Desc: Cria tabela temporaria para atualização dos saldos de Lote x Endereço.  //
// @Retorno: Nome tabela temporaria.                                              //
////////////////////////////////////////////////////////////////////////////////////

Static Function LxeTmp(cCodMP)
    Local   nLoop,x    :=  0
    Private aLxeInit := {}

    cCodMP := PADR(cCodMP,TamSx3('B1_COD')[1])
    //VTALERT("BP1")
    If Empty(cCodMP)
        VTALERT("Bug LXETMP. Codigo de produto invalido. SOLUÇÃO: Contate o administrador do sistema.")
        Return
    Endif
    
    cAliasName := "TMP_"+alltrim(cCodMP)

    /**Puxa estado Inicial Lote x Endereço**/
     IF SELECT("TMP") > 0
        TMP->(Dbclosearea())
    ENDIF

    cQry := "SELECT DISTINCT BF_LOCAL,BF_PRODUTO AS B8_PRODUTO,BF_LOTECTL AS B8_LOTECTL,BF_LOCALIZ,BF_QUANT AS B8_SALDO FROM "+RETSQLNAME("SBF")+" AS SBF WHERE BF_PRODUTO = '"+cCodMP+"' AND BF_QUANT > 0 AND SBF.D_E_L_E_T_ = ' ' AND BF_LOCAL = '01' "
    TCQUERY cQry NEW ALIAS "TMP"
    TCSETFIELD("TMP","B8_DATA","D",8,0)

    DbSelectArea("TMP")
    TMP->(Dbgotop())

    While !TMP->(Eof())
        //vtalert("b8_lotectl array alxe:"+TMP->B8_LOTECTL)
        aAdd(aLxeInit,{TMP->BF_LOCAL,TMP->B8_PRODUTO,TMP->B8_LOTECTL,TMP->BF_LOCALIZ,TMP->B8_SALDO})
        TMP->(DbSkip())
        
    EndDo 
    


    //Cria a temporária
    //Campos
    /* BF_LOCAL: C REAL 2
    B8_PRODUTO:C REAL 15
    B8_LOTE: C REAL 10
    BF_LOCALIZ: C REAL 15
    B8_SALDO: N REAL 14 2 */

    oTempTable := FWTemporaryTable():New(cAliasName)
    //vtalert("bp2")

    //Adiciona no array das colunas as que serão incluidas (Nome do Campo, Tipo do Campo, Tamanho, Decimais)
    aFields := {}
    aAdd(aFields, {"FILIAL", "C",TamSx3("B1_FILIAL")[1],0})
    aAdd(aFields, {"LOCALX",   "C", TamSx3('BF_LOCAL')[1], 0})
    aAdd(aFields, {"PRODUTO",   "C", TamSx3('B8_PRODUTO')[1], 0})
    aAdd(aFields, {"LOTE", "C", TamSx3('B8_LOTECTL')[1], 0})
    aAdd(aFields, {"LOCALIZ", "C", TamSx3('BF_LOCALIZ')[1], 0})
    aAdd(aFields, {"SALDO", "N", TamSx3('B8_SALDO')[1], 2})


 
    //Define as colunas usadas
    oTempTable:SetFields( aFields )

     //---------------------
    //Criação dos índices
    //---------------------
    oTempTable:AddIndex("01", {"FILIAL", "PRODUTO"} )

    //Efetua a criação da tabela
    oTempTable:Create()


     //------------------------------------
    //Pego o alias da tabela temporária
    //------------------------------------
    cAliasReal := oTempTable:GetAlias()

    //--------------------------------------------------------
    //Pego o nome real da tabela temporária no banco de dados
    //--------------------------------------------------------
    cTableName := oTempTable:GetRealName()
    

    //VTALERT("nomeTabela:"+cTablename)

    //VTALERT("Filial:"+FWFilial())

    //------------------------------
    //Inserção de dados para testes
    //------------------------------
    For x:=1 To len(aLxeInit)
        //LOCAL,PRODUTO,LOTE,END,SALDO
        //Padrão Alias: TMP_CODPROD
        (cAliasName)->(DBAppend())
        (cAliasName)->FILIAL  := FWFilial()
        (cAliasName)->LOCALX   := aLxeInit[x,1]
        (cAliasName)->PRODUTO := aLxeInit[x,2]
        (cAliasName)->LOTE    := aLxeInit[x,3]
        (cAliasName)->LOCALIZ := aLxeInit[x,4]
        (cAliasName)->SALDO   := aLxeInit[x,5]
        (cAliasName)->(DBCommit())
    End
        


    //Retorna nome da tabela, que será posteriormente adicionado a array.
    //Em momentos que houver necessidade de uso da tabela, será utilizado a aScan
    //para pegar a posição e nome tabela



Return cTableName




///////////////////////////////////////////////////////////////////////
//Função: LxeMxTmp()                                                 //
//Retorna o maximo disponível em estoque de acordo com o produto.    //
//Puxa da tabela temporária as informações.                          //
///////////////////////////////////////////////////////////////////////

Static Function LxEMxTmp(cCodMP)

    cAlias := "TMP_"+alltrim(cCodMP)
    nMxEtq := 0


    //Filial,Local,Produto,Lote,Localiz,Saldo
    //cQuerySQL := "SELECT FILIAL, LOCALX , PRODUTO , LOTE , LOCALIZ, SALDO FROM " + str(cTableName)
    //svtalert("passou cquery")
    //DBUseArea(.T., "TOPCONN", TCGenQry(,,cQuerySQL), cAlias, .T., .T.)
    (cAlias)->(DbGoTop())
    //vtalert("LxeMxTmp")

    while !(cAlias)->(Eof())
        nSaldo := (cAlias)->SALDO
        nMxEtq += (cAlias)->SALDO
        (cAlias)->(DBSkip())
    enddo



Return nMxEtq


/*/ Função: ClsTmp()
    Fecha tabelas/alias temporários que estejam em aberto.
    @type  Function
    @author Arinus K. de Oliveira
    @since 31/05/2022
    @Desc Loopa entre todos os alias em aberto utilizando a array aTmpNms,
    que grava todos os produtos que possuem alias temporario em aberto.
    /*/
Static Function ClsTmp()
    Local y

    //aTmpNMS := {Produto,Tabela,Alias}
    For y:=1 To Len(aTmpNms)
        cAliasNt := aTmpNms[y,3]
        (cAliasNt)->(DbCloseArea())
        //VTALERT("Alias:"+aTmpNms[y,3])

/*         If Select(cAliasNt) > 0
            VTALERT("Tabela ainda existe.. Erro")
        Else
            VTALERT("Alias "+alltrim(cAliasNt)+" foi fechado com sucesso.")
        Endif */
        
    End


Return 


