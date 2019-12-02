PROGRAM BOKU_NO_HERO

   IMPLICIT none
!===============================================================================
! Constantes para calcular o tempo de execucao e outros 
!===============================================================================
   INTEGER :: i, j, l, t, dia_i, dia_f, minuto_i, minuto_f, hora_f, hora_i, enc_X
   INTEGER :: dia_e, minuto_e, hora_e, segundo_e
   INTEGER :: segundo_i, segundo_f
   INTEGER, dimension (8) :: values
!===============================================================================
! Cosnstantes necessarias para a cadeia 
!===============================================================================
   INTEGER, PARAMETER :: tempo_final = 1.0D3, X_max = 10
   INTEGER :: quatidade_de_amostras 
   INTEGER, PARAMETER :: N = 3000
   DOUBLE PRECISION :: t_real
!===============================================================================
! Costantes da alta expasao 
!===============================================================================
   DOUBLE PRECISION, PARAMETER :: N_aut_exp = dble(N)
   DOUBLE PRECISION :: borda_i1i2, BOR
   INTEGER :: i1 = (nint(N_aut_exp/2.0D0) - 50)
   INTEGER :: i2 = (nint(N_aut_exp/2.0D0) + 50)
   INTEGER :: j1 = (nint(N_aut_exp/2.0D0) - 50)
   INTEGER :: j2 = (nint(N_aut_exp/2.0D0) + 50)
!===============================================================================
! Tempo
!===============================================================================
   DOUBLE PRECISION, PARAMETER :: dt = 1.0D-2, dX = 0.25
   INTEGER, PARAMETER :: tempo = tempo_final/dt + 1
   INTEGER, PARAMETER :: Pnt_f_enc_X = X_max/dX + 1
!===============================================================================
! Arquivos e Pasta
!===============================================================================
   CHARACTER (LEN=tempo) :: diretorio_EXT, diretorio_RTN, diretorio_DES
   CHARACTER (LEN=tempo) :: nome_EXT, nome_RTN, nome_DES
   CHARACTER (LEN=tempo) :: comando_EXT, comando_RTN, comando_DES
   CHARACTER (LEN=tempo) :: retorno, ext_espacial, desvio, temi
   CHARACTER (len=6) :: comando
!===============================================================================
! Ingredientes do sistema 
!===============================================================================
   DOUBLE PRECISION :: NL1, NL2, X, E(0:N+1), auxU, U
   DOUBLE COMPLEX :: f(0:N + 1,0:N + 1)
   DOUBLE PRECISION, PARAMETER :: V = 1.0D0   
   INTEGER :: semente, W
   REAL :: ran1
   INTEGER :: k, OLHA(tempo)
!===============================================================================
! Quantidades fisica
!===============================================================================
   DOUBLE PRECISION :: Part, Part_sobre_N, espacial, R(N)
   DOUBLE PRECISION :: cent1, cent2, ce1
!===============================================================================
! Funcoes Runge-Kutta 
!===============================================================================
   DOUBLE COMPLEX :: A, B, C
   DOUBLE COMPLEX, PARAMETER :: ic = (0 ,-1)*dt
   DOUBLE COMPLEX :: k1(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k2(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k3(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k4(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k5(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k6(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k7(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k8(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k9(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k10(0:N + 1,0:N + 1)
   DOUBLE COMPLEX :: k11(0:N + 1,0:N + 1)
!===============================================================================
! Constante Runge-Kutta
!===============================================================================
   DOUBLE PRECISION, PARAMETER :: s21=sqrt(21.0D0)
   DOUBLE PRECISION, PARAMETER :: k2a=1.0D0/2.0D0
   DOUBLE PRECISION, PARAMETER :: k3a=1.0D0/4.0D0
   DOUBLE PRECISION, PARAMETER :: k4a=1.0D0/7.0D0
   DOUBLE PRECISION, PARAMETER :: k4b=(-7.0D0-3.0D0*s21)/98.0D0
   DOUBLE PRECISION, PARAMETER :: k4c=(21.0D0+5.0D0*s21)/49.0D0
   DOUBLE PRECISION, PARAMETER :: k5a=(11.0D0+s21)/84.0D0
   DOUBLE PRECISION, PARAMETER :: k5b=(18.0D0+4.0D0*s21)/63.0D0
   DOUBLE PRECISION, PARAMETER :: k5c=(21.0D0-s21)/252.0D0
   DOUBLE PRECISION, PARAMETER :: k6a=(5.0D0+s21)/48.0D0
   DOUBLE PRECISION, PARAMETER :: k6b=(9.0D0+s21)/36.0D0
   DOUBLE PRECISION, PARAMETER :: k6c=(-231.0D0+14.0D0*s21)/360.0D0
   DOUBLE PRECISION, PARAMETER :: k6d=(63.0D0-7.0D0*s21)/80.0D0
   DOUBLE PRECISION, PARAMETER :: k7a=(10.0D0-s21)/42.0D0
   DOUBLE PRECISION, PARAMETER :: k7b=(-432.0D0+92.0D0*s21)/315.0D0
   DOUBLE PRECISION, PARAMETER :: k7c=(633.0D0-145.0D0*s21)/90.0D0
   DOUBLE PRECISION, PARAMETER :: k7d=(-504.0D0+115.0D0*s21)/70.0D0
   DOUBLE PRECISION, PARAMETER :: k7e=(63.0D0-13.0D0*s21)/35.0D0
   DOUBLE PRECISION, PARAMETER :: k8a=1.0D0/14.0D0
   DOUBLE PRECISION, PARAMETER :: k8b=(14.0D0-3.0D0*s21)/126.0D0
   DOUBLE PRECISION, PARAMETER :: k8c=(13.0D0-3.0D0*s21)/63.0D0
   DOUBLE PRECISION, PARAMETER :: k8d=1.0D0/9.0D0
   DOUBLE PRECISION, PARAMETER :: k9a=1.0D0/32.0D0
   DOUBLE PRECISION, PARAMETER :: k9b=(91.0D0-21.0D0*s21)/576.0D0
   DOUBLE PRECISION, PARAMETER :: k9c=11.0D0/72.0D0
   DOUBLE PRECISION, PARAMETER :: k9d=(-385.0D0-75.0D0*s21)/1152.0D0
   DOUBLE PRECISION, PARAMETER :: k9e=(63.0D0+13.0D0*s21)/128.0D0
   DOUBLE PRECISION, PARAMETER :: k10a=1.0D0/14.0D0
   DOUBLE PRECISION, PARAMETER :: k10b=1.0D0/9.0D0
   DOUBLE PRECISION, PARAMETER :: k10c=(-733.0D0-147.0D0*s21)/2205.0D0
   DOUBLE PRECISION, PARAMETER :: k10d=(515.0D0+111.0D0*s21)/504.0D0
   DOUBLE PRECISION, PARAMETER :: k10e=(-51.0D0-11.0D0*s21)/56.0D0
   DOUBLE PRECISION, PARAMETER :: k10f=(132.0D0+28.0D0*s21)/245.0D0
   DOUBLE PRECISION, PARAMETER :: k11a=(-42.0D0+7.0D0*s21)/18.0D0
   DOUBLE PRECISION, PARAMETER :: k11b=(-18.0D0+28.0D0*s21)/45.0D0
   DOUBLE PRECISION, PARAMETER :: k11c=(-273.0D0-53.0D0*s21)/72.0D0
   DOUBLE PRECISION, PARAMETER :: k11d=(301.0D0+53.0D0*s21)/72.0D0
   DOUBLE PRECISION, PARAMETER :: k11e=(28.0D0-28.0D0*s21)/45.0D0
   DOUBLE PRECISION, PARAMETER :: k11f=(49.0D0-7.0D0*s21)/18.0D0

   WRITE(comando,'(a)') "mkdir "
   
   PRINT *, "Digite o valor da interacao :"
   READ *, U

   CALL date_and_time(VALUES=values)
   dia_i = values(3)
   hora_i = values(5)      
   minuto_i = values(6)
   segundo_i = values(7)
!============== Abrindo arquivo pra esquever o tempo que comecou ===============
   WRITE (temi, '("Tempo_de_execucao_X",f5.2,"ate",i2,".txt")') X, X_max
   OPEN ( UNIT = 12, file = temi )
   WRITE (12, *) "Coloquei para rodar no dia ", dia_i, "na hora :", &
                                     hora_i, "h", minuto_i, "m", segundo_i, "s."
!============================== Fechando =======================================
   
   WRITE ( diretorio_EXT, '("EXTW",i1,"U",f4.2,"X0-",i2,"/")' ) W, U, X_max
   comando_EXT = comando // trim(diretorio_EXT)
   CALL system(comando_EXT)
   
   WRITE ( diretorio_RTN, '("RNTW",i1,"U",f4.2,"X0-",i2,"/")' ) W, U, X_max
   comando_RTN = comando // trim(diretorio_RTN)
   CALL system(comando_RTN)
   
!========================== loop das amostras ==================================
      X = 0.                                                                    ! Valor inicial
!      DO enc_X = 1, Pnt_f_enc_X                                                 ! Auto variacao de X
         OLHA(k) = semente
         semente = - semente
         t_real = 0.

!=============================== Abrindo arquivos ==============================
         WRITE (ext_espacial, '("EXTW",i1,"X",f4.2,"U",f6.2"AMT",i4,"N",i4 &
                                         ,"DT",f6.3,".dat" )') W, X, U, k, N, dt 
   
         WRITE (retorno, '("RTNW",i1,"X",f4.2,"U",f6.2,"AMT",i4,"N",i4 &
                                         ,"DT",f6.3,".dat" )') W, X, U, k, N, dt
!=========================== Fim de Abrir aquivos ==============================      

!============================= Pacote tipo delta ===============================
         f = (0.,0.)
         f(N/2,N/2) = (1., 0.)
!============================== loop do tempo ==================================
         DO t = 1, tempo 
!=============================== Runge-Kutta ===================================
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0            
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
!============================= Nao-linearidade =================================
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l)))**2
                     NL2 = NL2 + (ABS(f(l,j)))**2
                  END DO
                  k1(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*f(i,j) + &
                            V*(f(i + 1, j) + f(i - 1, j) + &
                               f(i, j - 1) + f(i, j + 1)))
               END DO
            END DO
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0            
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k2a*k1(i,l)))**2
                     NL2 = NL2 + (ABS(f(l,j) + k2a*k1(l,j)))**2
                  END DO
                  A = f(i,j) + k2a*k1(i,j)
                  B = f(i + 1, j) + k2a*k1(i + 1, j) + &
                      f(i, j + 1) + k2a*k1(i, j + 1)
                  C = f(i - 1, j) + k2a*k1(i - 1, j) + &
                      f(i, j - 1) + k2a*k1(i, j - 1)
                  k2(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO

            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0            
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k3a*( k1(i,l) + k2(i,l))))**2
                     NL2 = NL2 + (ABS(f(l,j) + k3a*( k1(l,j) + k2(l,j))))**2
                  END DO
                  A = f(i,j) + k3a*( k1(i,j) + k2(i,j))
                  B = f(i + 1, j) + k3a*( k1(i + 1, j) + k2(i + 1, j) ) + &
                      f(i, j + 1) + k3a*( k1(i, j + 1) + k2(i, j + 1) )
                  C = f(i - 1, j) + k3a*( k1(i - 1, j) + k2(i - 1, j) ) + &
                      f(i, j - 1) + k3a*( k1(i, j - 1) + k2(i, j - 1) )
                  k3(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO                   
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k4a + k2(i,l)*k4b + &
                           k3(i,l)*k4c))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k4a + k2(l,j)*k4b + &
                           k3(l,j)*k4c))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k4a + k2(i,j)*k4b + k3(i,j)*k4c
                  B = f(i+1,j) + k1(i+1,j)*k4a + k2(i+1,j)*k4b + k3(i+1,j)*k4c + &
                      f(i,j+1) + k1(i,j+1)*k4a + k2(i,j+1)*k4b + k3(i,j+1)*k4c
                  C = f(i-1,j) + k1(i-1,j)*k4a + k2(i-1,j)*k4b + k3(i-1,j)*k4c + &
                      f(i,j-1) + k1(i,j-1)*k4a + k2(i,j-1)*k4b + k3(i,j-1)*k4c
                  k4(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k5a + k3(i,l)*k5b + &
                           k4(i,l)*k5c))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k5a + k3(l,j)*k5b + &
                           k4(l,j)*k5c))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k5a + k3(i,j)*k5b + k4(i,j)*k5c
                  B = f(i+1,j) + k1(i+1,j)*k5a + k3(i+1,j)*k5b + k4(i+1,j)*k5c + &
                      f(i,j+1) + k1(i,j+1)*k5a + k3(i,j+1)*k5b + k4(i,j+1)*k5c
                  C = f(i-1,j) + k1(i-1,j)*k5a + k3(i-1,j)*k5b + k4(i-1,j)*k5c + &
                      f(i,j-1) + k1(i,j-1)*k5a + k3(i,j-1)*k5b + k4(i,j-1)*k5c
                  k5(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO

            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k6a + k3(i,l)*k6b + &
                           k4(i,l)*k6c + k5(i,l)*k6d))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k6a + k3(l,j)*k6b + &
                           k4(l,j)*k6c + k5(l,j)*k6d))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k6a + k3(i,j)*k6b + k4(i,j)*k6c + &
                       k5(i,j)*k6d
                  B = f(i+1,j) + k1(i+1,j)*k6a + k3(i+1,j)*k6b + k4(i+1,j)*k6c + &
                      k5(i+1,j)*k6d + &
                      f(i,j+1) + k1(i,j+1)*k6a + k3(i,j+1)*k6b + k4(i,j+1)*k6c + &
                      k5(i,j+1)*k6d
                  C = f(i-1,j) + k1(i-1,j)*k6a + k3(i-1,j)*k6b + k4(i-1,j)*k6c + &
                      k5(i-1,j)*k6d + &
                      f(i,j-1) + k1(i,j-1)*k6a + k3(i,j-1)*k6b + k4(i,j-1)*k6c + &
                      k5(i,j-1)*k6d
                  k6(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k7a + k3(i,l)*k7b + &
                           k4(i,l)*k7c + k5(i,l)*k7d + k6(i,l)*k7e))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k7a + k3(l,j)*k7b + &
                           k4(l,j)*k7c + k5(l,j)*k7d + k6(l,j)*k7e))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k7a + k3(i,j)*k7b + k4(i,j)*k7c + &
                      k5(i,j)*k7d + k6(i,j)*k7e
                  B = f(i+1,j) + k1(i+1,j)*k7a + k3(i+1,j)*k7b + k4(i+1,j)*k7c + &
                      k5(i+1,j)*k7d + k6(i+1,j)*k7e + &
                      f(i,j+1) + k1(i,j+1)*k7a + k3(i,j+1)*k7b + k4(i,j+1)*k7c + &
                      k5(i,j+1)*k7d + k6(i,j+1)*k7e
                  C = f(i-1,j) + k1(i-1,j)*k7a + k3(i-1,j)*k7b + k4(i-1,j)*k7c + &
                      k5(i-1,j)*k7d + k6(i-1,j)*k7e + &
                      f(i,j-1) + k1(i,j-1)*k7a + k3(i,j-1)*k7b + k4(i,j-1)*k7c + &
                      k5(i,j-1)*k7d + k6(i,j-1)*k7e
                  k7(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO     

            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k8a + k5(i,l)*k8b + &
                           k6(i,l)*k8c + k7(i,l)*k8d))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k8a + k5(l,j)*k8b + &
                           k6(l,j)*k8c + k7(l,j)*k8d))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k8a + k5(i,j)*k8b + k6(i,j)*k8c + &
                      k7(i,j)*k8d
                  B = f(i+1,j) + k1(i+1,j)*k8a + k5(i+1,j)*k8b + k6(i+1,j)*k8c + &
                      k7(i+1,j)*k8d + &
                      f(i,j+1) + k1(i,j+1)*k8a + k5(i,j+1)*k8b + k6(i,j+1)*k8c + &
                      k7(i,j+1)*k8d
                  C = f(i-1,j) + k1(i-1,j)*k8a + k5(i-1,j)*k8b + k6(i-1,j)*k8c + &
                      k7(i-1,j)*k8d + &
                      f(i,j-1) + k1(i,j-1)*k8a + k5(i,j-1)*k8b + k6(i,j-1)*k8c + &
                      k7(i,j-1)*k8d
                  k8(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO  
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k9a + k5(i,l)*k9b + &
                           k6(i,l)*k9c + k7(i,l)*k9d + k8(i,l)*k9e))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k9a + k5(l,j)*k9b + &
                           k6(l,j)*k9c + k7(l,j)*k9d + k8(l,j)*k9e))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k9a + k5(i,j)*k9b + k6(i,j)*k9c + &
                      k7(i,j)*k9d + k8(i,j)*k9e
                  B = f(i+1,j) + k1(i+1,j)*k9a + k5(i+1,j)*k9b + k6(i+1,j)*k9c + &
                      k7(i+1,j)*k9d + k8(i+1,j)*k9e + &
                      f(i,j+1) + k1(i,j+1)*k9a + k5(i,j+1)*k9b + k6(i,j+1)*k9c + &
                      k7(i,j+1)*k9d + k8(i,j+1)*k9e
                  C = f(i-1,j) + k1(i-1,j)*k9a + k5(i-1,j)*k9b + k6(i-1,j)*k9c + &
                      k7(i-1,j)*k9d + k8(i-1,j)*k9e + &
                      f(i,j-1) + k1(i,j-1)*k9a + k5(i,j-1)*k9b + k6(i,j-1)*k9c + &
                      k7(i,j-1)*k9d + k8(i,j-1)*k9e
                  k9(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO 
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k1(i,l)*k10a + k5(i,l)*k10b + &
                           k6(i,l)*k10c + k7(i,l)*k10d + k8(i,l)*k10e + &
                           k9(i,l)*k10f))**2
                     NL2 = NL2 + (ABS(f(l,j) + k1(l,j)*k10a + k5(l,j)*k10b + &
                           k6(l,j)*k10c + k7(l,j)*k10d + k8(l,j)*k10e + &
                           k9(l,j)*k10f))**2
                  END DO
                  A = f(i,j) + k1(i,j)*k10a + k5(i,j)*k10b + k6(i,j)*k10c + &
                      k7(i,j)*k10d + k8(i,j)*k10e + k9(i,j)*k10f
                  B = f(i+1,j) + k1(i+1,j)*k10a + k5(i+1,j)*k10b + k6(i+1,j)*k10c &
                      + k7(i+1,j)*k10d + k8(i+1,j)*k10e + k9(i+1,j)*k10f + &
                      f(i,j+1) + k1(i,j+1)*k10a + k5(i,j+1)*k10b + k6(i,j+1)*k10c &
                      + k7(i,j+1)*k10d + k8(i,j+1)*k10e + k9(i,j+1)*k10f
                  C = f(i-1,j) + k1(i-1,j)*k10a + k5(i-1,j)*k10b + k6(i-1,j)*k10c &
                      + k7(i-1,j)*k10d + k8(i-1,j)*k10e + k9(i-1,j)*k10f + &
                      f(i,j-1) + k1(i,j-1)*k10a + k5(i,j-1)*k10b + k6(i,j-1)*k10c &
                      + k7(i,j-1)*k10d + k8(i,j-1)*k10e + k9(i,j-1)*k10f
                  k10(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO 
            
            DO i = i1, i2
               DO j = j1, j2
                  auxU = 0D0
                  NL1 = 0D0         
                  NL2 = 0D0
                  IF( i==j ) auxU = 1d0
                  DO  l = i1, i2
                     NL1 = NL1 + (ABS(f(i,l) + k5(i,l)*k11a + k6(i,l)*k11b + &
                           k7(i,l)*k11c + k8(i,l)*k11d + k9(i,l)*k11e + &
                           k10(i,l)*k11f))**2
                     NL2 = NL2 + (ABS(f(l,j) + k5(l,j)*k11a + k6(l,j)*k11b + &
                           k7(l,j)*k11c + k8(l,j)*k11d + k9(l,j)*k11e + &
                           k10(l,j)*k11f))**2
                  END DO
                  A = f(i,j) + k5(i,j)*k11a + k6(i,j)*k11b + k7(i,j)*k11c + &
                      k8(i,j)*k11d + k9(i,j)*k11e + k10(i,j)*k11f
                  B = f(i+1,j) + k5(i+1,j)*k11a + k6(i+1,j)*k11b + k7(i+1,j)*k11c &
                      + k8(i+1,j)*k11d + k9(i+1,j)*k11e + k10(i+1,j)*k11f + &
                      f(i,j+1) + k5(i,j+1)*k11a + k6(i,j+1)*k11b + k7(i,j+1)*k11c &
                      + k8(i,j+1)*k11d + k9(i,j+1)*k11e + k10(i,j+1)*k11f
                  C = f(i-1,j) + k5(i-1,j)*k11a + k6(i-1,j)*k11b + k7(i-1,j)*k11c &
                      + k8(i-1,j)*k11d + k9(i-1,j)*k11e + k10(i-1,j)*k11f + &
                      f(i,j-1) + k5(i,j-1)*k11a + k6(i,j-1)*k11b + k7(i,j-1)*k11c &
                      + k8(i,j-1)*k11d + k9(i,j-1)*k11e + k10(i,j-1)*k11f
                  k11(i,j) = ic*((X*(NL1 + NL2) + auxU*U)*A + &
                                 V*(B + C))
               END DO
            END DO   
            
            DO i = i1, i2
               DO j = j1, j2
                  f(i,j) = f(i,j) + (9.0D0*(k1(i,j) + k11(i,j)) + 49.0D0*(k8(i,j) &
                           + k10(i,j)) + 64.0D0*k9(i,j))/180.0D0
               END DO
            END DO
!============================= Fim do Runge-Kutta ==============================

!================================== Bordas =====================================
            BOR = 0.0D0
            DO i = i1, i2
               DO j = j1, j2
                  BOR = BOR + (ABS(f(i1,j)))**2 + (ABS(f(i2,j)))**2 + &
                        (ABS(f(i,j1)))**2 + (ABS(f(i,j2)))**2
               END DO
            END DO
            borda_i1i2 = ABS( i2 - i1 )
            IF ( BOR >= 1.0D-50 .AND. borda_i1i2 < N ) THEN
               i1 = i1 - 10
               i2 = i2 + 10
               j1 = j1 - 10
               j2 = j2 + 10
            END IF
!============================== Fim da bordas ==================================

!============================ Extensao Espacial ================================
            nome_EXT = trim(diretorio_EXT) // trim(ext_espacial)
            OPEN (UNIT = 10, file = nome_EXT)
            cent1 = 0.
            cent2 = 0.
            DO i = i1, i2
               DO j = j1, j2
                  cent1 = cent1 + (ABS(f(i,j))**2)*i
                  cent2 = cent2 + (ABS(f(i,j))**2)*j
!                  ce1 = ce1 + (ABS(f(i,j))**2)*i**2
!                  ce2 = ce2 + (ABS(f(i,j))**2)*j**2
               END DO
            END DO
!            desvio2_p1 = ce1 - cent1**2
            espacial=0.
            DO i = i1, i2
               DO j = j1, j2
                  espacial = espacial + &
                           ( sqrt((i - cent1)**2 + (j - cent2)**2) )*ABS(f(i,j))**2
               END DO
            END DO
!============================= Fim da Extensao =================================
  
!============================ Probabilidae de retorno ==========================
            nome_RTN = trim(diretorio_RTN) // trim(retorno)
            OPEN (UNIT = 9, file = nome_RTN)
            R(i) = (abs(f(N/2,N/2)))**2.
!======================== Fim da probabilidae de retorno =======================
            WRITE (10, *) t_real, espacial                                      ! extenção espacial
            WRITE (9, *) t_real, R(i)                                           ! Probabilidade de Retorno
!==================================== tempo ====================================
            t_real = t_real + dt
!========================== Fim de escrever dados ==============================
         END DO
!==================== Escrevendo hora que termina uma a mostra =================
         CALL date_and_time(VALUES=values)
         dia_e = values(3)
         hora_e = values(5)      
         minuto_e = values(6)
         segundo_e = values(7)      
         WRITE (12, *) "Encremento", X,"terminal no ", dia_e, "na hora:", &
                                     hora_e, "h", minuto_e, "m", segundo_e, "s." 
!======================== Fim de escrever hora que termina =====================
!         X = X + dX
!      END DO

END PROGRAM BOKU_NO_HERO

FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      REAL ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1./IM,IQ=127773,IR=2836)
      PARAMETER (NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.-EPS)

      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
      idum=max(-idum,1)
      do 11 j=NTAB+8,1,-1
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k


      if (idum.lt.0) idum=idum+IM
      if (j.le.NTAB) iv(j)=idum
11    continue
      iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
      return
      END
          
