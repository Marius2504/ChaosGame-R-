library(rmarkdown)
library(shiny)

x1 =sample(1:100,1)
x2 =sample(1:100,1)
x3 =sample(1:100,1)
x4 =sample(1:100,1)
x5 =sample(1:100,1)
y1 =sample(1:100,1)
y2 =sample(1:100,1)
y3 =sample(1:100,1)
y4 =sample(1:100,1)
y5 =sample(1:100,1)


vx=c(x1,x2,x3,x4)
vy=c(y1,y2,y3,y4)
varf=0
ui <- fluidPage(
  sliderInput(inputId="num", label="Puncte",
              value=4, min = 0, max = 5000),
  radioButtons("radio", label = "Optiuni",
               choices = list("triunghi" = 1, "patrat1" = 2,"patrat2" = 3,"patrat3" = 4, "patrat4" = 5),selected = 2),
  
  plotOutput("hist")
)

server <- function(input, output){
    output$hist <- renderPlot({
      if({input$radio}!=1){
        assign("y2",y1,envir = .GlobalEnv)
        assign("x3",x1,envir = .GlobalEnv)
        assign("y3",abs(x1-x2+y1),envir = .GlobalEnv)
        assign("x4",x2,envir = .GlobalEnv)
        assign("y4",abs(x1-x2+y1),envir = .GlobalEnv)
        assign("vx",vx[-1:-4],envir = .GlobalEnv)
        assign("vy",vy[-1:-4],envir = .GlobalEnv)
        assign("vx",c(x1,x2,x3,x4,vx),envir = .GlobalEnv)
        assign("vy",c(y1,y2,y3,y4,vy),envir = .GlobalEnv)
      }
      if({input$radio}==1){
        start({input$num})
      }
      else if({input$radio}==2)
      { 
        startt1({input$num})
      }
      else if({input$radio}==3)
      {
        startt2({input$num})
      }
      else if({input$radio}==4)
      {
        startt3({input$num})
      }
      else
      {
        startt4({input$num})
      }
      plot(
        x=vx,
        y=vy,
        type ="p",
        main="CHAOS GAME",
        xlab="Axa x",
        ylab="Axa y",
        xlim =c(0,100),
        ylim =c(0,100),
        col = "blue",
        pch = 16,
        cex = 1
      )
    },
    width = 800,
    height = 800)
}
shinyApp(ui = ui, server = server)

start<-function(maxi){
  i=length(vx)
  while (i<maxi) {
    rnd =sample(1:3,1)
    if(rnd == 1)
    {
      xm=(x1+x4)/2
      ym=(y1+y4)/2
    }
    else if(rnd ==2)
    {
      xm=(x2+x4)/2
      ym=(y2+y4)/2
    }
    else{
      xm=(x3+x4)/2
      ym=(y3+y4)/2
    }
    xm=floor(xm)
    ym=floor(ym)
    assign("x4",xm,envir = .GlobalEnv)
    assign("y4",ym,envir = .GlobalEnv)
    assign("vx",c(vx,xm),envir = .GlobalEnv)
    assign("vy",c(vy,ym),envir = .GlobalEnv)
    
    i=i+1
  }
  if (i>maxi) 
  {
    assign("vx",vx[-maxi:-(length(vx))],envir = .GlobalEnv)
    assign("vy",vy[-maxi:-(length(vy))],envir = .GlobalEnv)
  }
}


startt1 <-function(maxi){
  i=length(vx)
  while (i<maxi) {
    rnd =sample(1:4,1)
    if(rnd == 1)
    {
      xm=(x1+x5)/2
      ym=(y1+y5)/2
    }
    else if(rnd ==2)
    {
      xm=(x2+x5)/2
      ym=(y2+y5)/2
    }
    else if (rnd ==3){
      xm=(x3+x5)/2
      ym=(y3+y5)/2
    }
    else
    {
      xm=(x4+x5)/2
      ym=(y4+y5)/2
    }
    xm=floor(xm)
    ym=floor(ym)
    assign("x5",xm,envir = .GlobalEnv)
    assign("y5",ym,envir = .GlobalEnv)
    assign("vx",c(vx,xm),envir = .GlobalEnv)
    assign("vy",c(vy,ym),envir = .GlobalEnv)
    assign("varf",rnd,envir = .GlobalEnv)
    
    i=i+1
  }
  if (i>maxi) 
  {
    assign("vx",vx[-maxi:-(length(vx))],envir = .GlobalEnv)
    assign("vy",vy[-maxi:-(length(vy))],envir = .GlobalEnv)
  }
}


startt2 <-function(maxi){
  i=length(vx)
  while (i<maxi) {
    rnd =sample(1:4,1)
    #nu trebuie ales acelasi varf de doua ori
    while(rnd==varf){
      rnd =sample(1:4,1)
    }
    #-------
    if(rnd == 1)
    {
      xm=(x1+x5)/2
      ym=(y1+y5)/2
    }
    else if(rnd ==2)
    {
      xm=(x2+x5)/2
      ym=(y2+y5)/2
    }
    else if (rnd ==3){
      xm=(x3+x5)/2
      ym=(y3+y5)/2
    }
    else
    {
      xm=(x4+x5)/2
      ym=(y4+y5)/2
    }
    xm=floor(xm)
    ym=floor(ym)
    assign("x5",xm,envir = .GlobalEnv)
    assign("y5",ym,envir = .GlobalEnv)
    assign("vx",c(vx,xm),envir = .GlobalEnv)
    assign("vy",c(vy,ym),envir = .GlobalEnv)
    assign("varf",rnd,envir = .GlobalEnv)
    
    i=i+1
  }
  if (i>maxi) 
  {
    assign("vx",vx[-maxi:-(length(vx))],envir = .GlobalEnv)
    assign("vy",vy[-maxi:-(length(vy))],envir = .GlobalEnv)
  }
}


startt3 <-function(maxi){
  i=length(vx)
  while (i<maxi) {
    rnd =sample(1:4,1)
    #nu trebuie ales acelasi varf de doua ori
    while((rnd==1 && varf==4)||(rnd==2 && varf==3)||(rnd==3 && varf==2)||(rnd==4 && varf==1)){
      rnd =sample(1:4,1)
    }
    
    #-------
    if(rnd == 1)
    {
      xm=(x1+x5)/2
      ym=(y1+y5)/2
    }
    else if(rnd ==2)
    {
      xm=(x2+x5)/2
      ym=(y2+y5)/2
    }
    else if (rnd ==3){
      xm=(x3+x5)/2
      ym=(y3+y5)/2
    }
    else
    {
      xm=(x4+x5)/2
      ym=(y4+y5)/2
    }
    xm=floor(xm)
    ym=floor(ym)
    assign("x5",xm,envir = .GlobalEnv)
    assign("y5",ym,envir = .GlobalEnv)
    assign("vx",c(vx,xm),envir = .GlobalEnv)
    assign("vy",c(vy,ym),envir = .GlobalEnv)
    assign("varf",rnd,envir = .GlobalEnv)
    
    i=i+1
  }
  if (i>maxi) 
  {
    assign("vx",vx[-maxi:-(length(vx))],envir = .GlobalEnv)
    assign("vy",vy[-maxi:-(length(vy))],envir = .GlobalEnv)
  }
}


startt4 <-function(maxi){
  i=length(vx)
  while (i<maxi) {
    rnd =sample(1:4,1)
    #nu trebuie ales acelasi varf de doua ori
    while((rnd==1 && varf==3)||(rnd==2 && varf==1)||(rnd==3 && varf==4)||(rnd==4 && varf==2)){
      rnd =sample(1:4,1)
    }
    
    #-------
    if(rnd == 1)
    {
      xm=(x1+x5)/2
      ym=(y1+y5)/2
    }
    else if(rnd ==2)
    {
      xm=(x2+x5)/2
      ym=(y2+y5)/2
    }
    else if (rnd ==3){
      xm=(x3+x5)/2
      ym=(y3+y5)/2
    }
    else
    {
      xm=(x4+x5)/2
      ym=(y4+y5)/2
    }
    xm=floor(xm)
    ym=floor(ym)
    assign("x5",xm,envir = .GlobalEnv)
    assign("y5",ym,envir = .GlobalEnv)
    assign("vx",c(vx,xm),envir = .GlobalEnv)
    assign("vy",c(vy,ym),envir = .GlobalEnv)
    assign("varf",rnd,envir = .GlobalEnv)
    
    i=i+1
  }
  if (i>maxi) 
  {
    assign("vx",vx[-maxi:-(length(vx))],envir = .GlobalEnv)
    assign("vy",vy[-maxi:-(length(vy))],envir = .GlobalEnv)
  }
}

#------------------------------------------------------------------------------#


