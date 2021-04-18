import pandas as pd
df=pd.read_excel('template.xlsx',dtype={'date':str,'tc_flag':str,'ad_flag':str,'time_window':str})


#设置分析时间窗,依次为投前，投中
time={'311':[['20210308','20210310'],['20210311','20210311']],
      '319':[['20210312','20210317'],['20210318','20210320']]}
#设置T&C人数
tc={'T|PINGPAI|311':1000,
    'C|PINGPAI|311':500,
    'T|jingjia|311':800,
    'C|jingjia|311':200,

    'T|PINGPAI|319': 1000,
    'C|PINGPAI|319': 500,
    'T|jingjia|319': 800,
    'C|jingjia|319': 200,
    }
# def aggregation(df):
#     pre=time[list(df['time_window'].unique())[0]][0]
#     mid=time[list(df['time_window'].unique())[0]][1]
#     target=list(df.columns)[4:]
#     return df2

def create_table(df):
    column_name=['T','C','对照组打平','对照组DID','对照组线性']
    row_name=['曝光人数']
    for i in list(df.columns)[4:]:
        row_name.append(i+'-投前')
        row_name.append(i)
    # row_name=row_name+list(df.columns)[4:]
    tc_flag=list(df['tc_flag'].unique())
    if len(tc_flag)>2:
        tc_flag.remove('T')
        tc_flag.remove('C')
        while len(tc_flag)>0:
            column_name.append(tc_flag[0])
            tc_flag.pop()
    return pd.DataFrame(index=row_name,columns=column_name)

table={}
for i in list(df['time_window'].unique()):
    #df_others=pd.DataFrame()
    for j in list(df[df['time_window']==i]['ad_flag'].unique()): #填非others表
        if j=='OTHERS':
            continue
        temp=i+j
        df_temp=df[(df['time_window']==i)&(df['ad_flag']==j)]
        table[temp]=create_table(df_temp)
        for p in list(df_temp['tc_flag'].unique()):
            if p == 'T' or p == 'C':
                table[temp].loc['曝光人数',p]=tc[p+'|'+j+'|'+i]
            for z in list(df.columns)[4:]:
                df_tempp=df_temp[df_temp['tc_flag']==p]
                table[temp].loc[z, p]=df_tempp[(df_tempp['date']>=time[i][1][0])&(df_tempp['date']<=time[i][1][1])][z].mean()
                table[temp].loc[z+'-投前', p]=df_tempp[(df_tempp['date']>=time[i][0][0])&(df_tempp['date']<=time[i][0][1])][z].mean()
            # table[temp].loc['']

    for j in list(df[df['time_window']==i]['ad_flag'].unique()): #填others
        if j!='OTHERS':
            continue
        df_temp = df[(df['time_window'] == i) & (df['ad_flag'] == j)]
        for temp in list(table.keys()):
            if not(i in temp):
                continue
            table[temp]['OTHERS'] = ''
            for z in list(df.columns)[4:]:
                table[temp].loc[z, 'OTHERS'] = df_temp[(df_temp['date'] >= time[i][1][0]) & (df_temp['date'] <= time[i][1][1])][z].mean()
                table[temp].loc[z + '-投前', 'OTHERS'] = df_temp[(df_temp['date'] >= time[i][0][0]) & (df_temp['date'] <= time[i][0][1])][z].mean()

##对照组调整
for temp in list(table.keys()):
    num_t=table[temp].loc['曝光人数','T']
    num_c = table[temp].loc['曝光人数', 'C']
    for i in range(1,len(table[temp])): ##对照组打平
        table[temp].iloc[i,2]=table[temp].iloc[i,1]*num_t/num_c
    for z in list(df.columns)[4:]:
        #对照组DID
        table[temp].loc[z, '对照组DID']=table[temp].loc[z, '对照组打平']+table[temp].loc[z+'-投前', 'T']-table[temp].loc[z+'-投前', '对照组打平']
        table[temp].loc[z+'-投前', '对照组DID'] = table[temp].loc[z+'-投前', '对照组打平'] + table[temp].loc[z + '-投前', 'T'] - table[temp].loc[
            z + '-投前', '对照组打平']
        #对照组线性
        table[temp].loc[z, '对照组线性']=table[temp].loc[z, '对照组打平']*table[temp].loc[z+'-投前', 'T']/table[temp].loc[z+'-投前', '对照组打平']
        table[temp].loc[z+'-投前', '对照组线性'] = table[temp].loc[z+'-投前', '对照组打平']*table[temp].loc[z + '-投前', 'T'] / table[temp].loc[
            z + '-投前', '对照组打平']
    # 计算rate
    ratelist=[]
    for z in list(df.columns)[4:]:
        ratelist.append(z)
        if len(ratelist)>1:
            table[temp].loc[ratelist[-1]+'/'+ratelist[-2]]=table[temp].loc[ratelist[-1]]/table[temp].loc[ratelist[-2]]
#计算rate

with pd.ExcelWriter('table.xlsx') as writer:
    for i in list(table.keys()):
        table[i].to_excel(writer,sheet_name=i)