#调用百度地图api，输入起点与终点坐标获取路线

#2016-05-11

#参考
##web api说明
# http://lbsyun.baidu.com/index.php?title=webapi/direction-api
##坐标反查
# http://api.map.baidu.com/lbsapi/getpoint/index.html
##开发资源
# http://lbsyun.baidu.com/index.php?title=open/dev-res
##R语言调用百度地图
# http://blog.sina.com.cn/s/blog_4b3c1a8801A02v0hy.html

# http://blog.fens.me/r-app-china-weather/

# 载入需要的包
##RCurl用于捕获网页内容,rjson用于处理json对象
library('rjson')
library('readxl')
library('RCurl')
library('XML')
library('xml2')
library('stringr')#字符串处理，包括正则表达式
setwd('E:\\分析\\网点布局')
#设置文件夹
#setwd('D:\\GWwork\\myR\\baidumap')
#读取文件
source('E:\\备件\\库存分析\\之前报告\\没有处理的原始路线\\StringAll(1).R', encoding='UTF-8')

data_df=read_excel('城市到城市.xlsx',sheet=1)

#ak_char="GpwCernS843svPgZp6k7G3PP"
#ak_char="njeQGVQFEsyDgB5Es3j1hUyGcjkKUtWq"
#ak_char="iItErxZPxZrDgVW2dCjAu2TysI0nYqj6"
ak_char="erEdovxFrLrYzcB2C17cb1luZTUkvLo7"
#ak_char="3oGVmHZNKaexFkeZUKsMlyFmE87uGTU9"
#预定义路线列
data_df[,'状态']=rep('0',nrow(data_df))
data_df[,'距离']=rep(0,nrow(data_df))
data_df[,'时间']=rep(0,nrow(data_df))
#####data_df[,'路线']=rep('0',nrow(data_df))
#循环处理
for (i0 in 1:nrow(data_df)){   #1:nrow(data_df)
    #读取参数
  origin_char=paste(data_df$起点纬度[i0],data_df$起点经度[i0],sep=',')
#     origin_region_char=data_df$起点城市[i0]
    destination_char=paste(data_df$终点纬度[i0],data_df$终点经度[i0],sep=',')
#     destination_region_char=data_df$终点城市[i0]
    tactics_char=data_df$导航策略[i0]
  #导航策略处理  
  if (tactics_char=='不走高速'){
    tactics_num=10
  }else if(tactics_char=='最少时间'){
    tactics_num=11
  }else if(tactics_char=='最短路径'){
    tactics_num=12
  }


  #获取起点城市信息
#  url_string<-paste("http://api.map.baidu.com/cloudrgc/v1?",
#                    "location=",origin_char,
#                    "&geotable_id=140378&coord_type=bd09ll",
#                    "&ak=",ak_char,
#                    sep='')
#  line_char = getURL(url_string)#url_string<-URLencode(url_string)
#  dele1=strsplit(line_char,":")[[1]]
#  city.char<-strsplit(dele1[10],"\"")[[1]][2]
city.char <- NA
data_df$起点城市[i0]=city.char
  ## 捕获连接对象
origin_region_char=city.char
#获取终点城市信息
#url_string<-paste("http://api.map.baidu.com/cloudrgc/v1?",
#                  "location=",destination_char,
#                  "&geotable_id=140378&coord_type=bd09ll",
#                  "&ak=",ak_char,
#                  sep='')
#line_char = getURL(url_string)#url_string<-URLencode(url_string)
#dele1=strsplit(line_char,":")[[1]]
#city.char<-strsplit(dele1[10],"\"")[[1]][2]
city.char <- NA
data_df$终点城市[i0]=city.char
## 捕获连接对象
destination_region_char=city.char



  # 获取路线信息:字符拼接并重编码，注意ak要换成自己的，可以在百度地图api免费申请
  url_string<-paste("http://api.map.baidu.com/direction/v1?mode=driving",
                    "&origin=",origin_char,
                    "&destination=",destination_char,
                    "&origin_region=",origin_region_char,
                    "&destination_region=",destination_region_char,
                    "&tactics=",tactics_num,
                    "&output=xml&",
                    "ak=",ak_char,
                    sep='')



  url_string<-URLencode(url_string)
  # 捕获连接对象
  result_xml<-read_xml(url_string)
#result_xml <- htmlParse(url_string)
#rootNode <- xmlRoot(result_xml)
  # 提取信息
  result_list=as_list(result_xml)
  ##状态码，0表示正常
  status.num=as.numeric(result_list$status)
  if(status.num==1) next
  data_df[i0,'状态']=ifelse(status.num==0,'正常',
                        ifelse(status.num==2,'参数错误',
                               ifelse(status.num==5,'权限或配额校验失败',status.num)))
  ##距离,千米
  distance.num=as.numeric(result_list$result$routes$distance)/1000
  data_df[i0,'距离']=distance.num
  ##时间，小时
  duration.num=as.numeric(result_list$result$routes$duration)/3600
  data_df[i0,'时间']=duration.num
  ##路程说明
 #####ins_char=xml_text(xml_find_all(result_xml, "//instructions"))
 #####ins_char=iconv(ins_char,"UTF-8", "UTF-8")
  ###去除多余字符
  #####ins_char=str_replace_all(ins_char,'<b>','')
 #####ins_char=str_replace_all(ins_char,'</b>','')
#####ins_char=str_replace_all(ins_char,'</font>','')
#####ins_char=str_replace_all(ins_char,'<br/>','')
#####ins_char=str_replace_all(ins_char,'<font color=\".*\"','')
#####ins_char=str_replace_all(ins_char,'>','')
#####ins_char=StringAll(x=ins_char,code=';')
#####data_df[i0,'路线']=ins_char
  rm(result_xml,duration.num)
  print(i0)
}
#输出
write.csv(data_df,'仓库到仓库输出.csv', row.names = F)

