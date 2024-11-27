ss3.make.filename = function(file,path="")
{
  if(path != "") {
    plc<-substring(path, nchar(path))
    if(!(plc== "\\" | plc=="/")) path <- paste(path, "/", sep = "")
  }
  filename<-paste(path, file, sep = "")
  return(filename)
}


ss3.convert.to.lines = function(filename)
{
  scan(filename, what = "", sep = "\n")
}
ss3.get.lines = function(lines, from = -1, to = -1, contains = "", starts.with = "", clip.to = "", clip.from = "", clip.to.match = "", clip.from.match = "", ...)
{
  result <- lines
  if(from > 0) {
    result <- result[(1:length(result)) >= from]
  }
  if(to > 0) {
    result <- result[(1:length(result)) <= to]
  }
  if(clip.to != "") {
    result <- result[(ss3.pos(result, clip.to) + 1):length(result)]
  }
  if(clip.from != "") {
    result <- result[1:(ss3.pos(result, clip.from) - 1)]
  }
  if(clip.to.match != "") {
    if(ss3.regexp.in(result, clip.to.match)) {
      result <- result[(ss3.pos.match(result, clip.to.match) + 1):length(result)]
    }
  }
  if(clip.from.match != "") {
    if(ss3.regexp.in(result, clip.from.match)) {
      result <- result[1:(ss3.pos.match(result, clip.from.match) - 1)]
    }
  }
  if(contains != "") {
    result <- result[ss3.regexpr(contains, result) > 0]
  }
  if(starts.with != "") {
    result <- result[ss3.regexpr(starts.with, result) > 0]
  }
  return(result)
}

ss3.pos = function(vector, x)
{
  min((1:length(vector))[vector == x])
}


ss3.pos.match = function(vector, regexp)
{
  min((1:length(vector))[ss3.regexpr(regexp, vector) > 0])
}

ss3.regexpr = function(x, y,fixed=T)
{
    return(regexpr(x, y, fixed=fixed))
}

ss3.regexp.in = function(vector, regexp)
{
  if(length(vector) == 0)
    return(F)
  any(ss3.regexpr(regexp, vector) > 0)
}


extract.derived_quantities = function (file='report.sso', path = "") {
	
	der = list()
	filename = ss3.make.filename(path = path, file = file)
	file = ss3.convert.to.lines(filename)
	file = ss3.get.lines(file, clip.from = " MGparm_By_Year_after_adjustments report:7")
	file = ss3.get.lines(file, clip.to = " DERIVED_QUANTITIES report:6")
    file = ss3.get.lines(file, clip.to.match = "B_ratio_denominator:  100%*B_MSY")
	#file = ss3.get.lines(file, clip.to = "DERIVED_QUANTITIES")
	#file = ss3.get.lines(file, clip.from = "MGparm_By_Year_after_adjustments")
	#file = ss3.get.lines(file, clip.to.match = "B_ratio_denominator:")
	file = sub('SPB','SSB',file)
	file = sub('LABEL','Label',file)
	der = read.table(textConnection(file),header=T,fill=T)	
	
}


extract.lik = function (file='report.sso', path = "") {
	
	der = list()
	filename = ss3.make.filename(path = path, file = file)
	file = ss3.convert.to.lines(filename)
	file = ss3.get.lines(file, clip.to.match = "Component logL*Lambda Lambda")
	file = ss3.get.lines(file, clip.from.match = "Crash_Pen")
	file = c('Value Lambda',file)
	der = read.table(textConnection(file),header=T,fill=T)	
	
}

  
 
 

ppath <- function(path,file) {
 return(paste(path,file,sep='/'))
}

load_obj <- function(dir,dat = 'data.ss',ctl='control.ss',starter='starter.ss',forecast='forecast.ss',parstart,version='3.30') {
  obj <- list()
  obj$dir <- dir
  obj$dat <- SS_readdat(make.filename(dat,dir),version=version)
  #obj$dat <- readLines(make.filename(dat,dir))
  obj$ctl <- readLines(make.filename(ctl,dir))
  obj$starter <- SS_readstarter(make.filename(starter,dir))
  obj$forecast <- readLines(make.filename(forecast,dir))
  if(!missing(parstart)) obj$parstart <- readLines(make.filename(parstart,dir))
  return(obj)
}

make_run <- function(obj,SSsource,dat = 'data.ss',ctl='control.ss',starter='starter.ss',forecast='forecast.ss') {
  dir.create(obj$dir)
  if(!missing(SSsource)) file.copy(from=make.filename("ss.exe",SSsource),to=obj$dir)
  if(exists("parstart",where=obj)) write(obj$parstart,file=make.filename("ss.par",obj$dir))
  write(obj$ctl,file=make.filename(ctl,obj$dir))
  write(obj$forecast,file=make.filename(forecast,obj$dir))
  #write(obj$dat,file=make.filename(dat,obj$dir))
  SS_writedat(obj$dat,make.filename(dat,obj$dir),version = "3.30",overwrite=T)
  SS_writestarter(obj$starter,dir=obj$dir,file=starter,overwrite=T)
}

