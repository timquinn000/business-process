DECLARE @START_TIME AS datetime2;
DECLARE @END_TIME AS datetime2;
SELECT @START_TIME = '2021-01-01';
SELECT @END_TIME = '2021-12-01';

with stec as (
  select 
    cms.[Carton meat STEC ID] as 'STEC ID'
    ,cms.[Production date]
    ,cms.[Test date]
    ,cms.[Confirmation date]
    ,cms.Destination
  from 
    [T2_EXPORTS].[MEDC].[CartonMeatStec] cms
  where 1=1
    --and cms.[Carton meat STEC ID] = 129287
    and cms.[Production date]>=@START_TIME
    and cms.[Production date]<@END_TIME
)

select 
  stec.[STEC ID] 
  ,'Production' as 'Activity'
  ,stec.[Production date] as 'Start datetime'
  ,stec.Destination as 'Destination'
  ,'Start' as 'Status'
from 
  stec
where
  stec.[Production date] is not null

union all

select 
  stec.[STEC ID] 
  ,'Test' 
  ,stec.[Test date]
  ,stec.Destination 
  ,'Start'
from 
  stec
where
  stec.[Test date] is not null

union all

select 
  stec.[STEC ID] 
  ,'Confirmed' 
  ,stec.[Confirmation date]
  ,stec.Destination 
  ,'Start'
from 
  stec
where
  stec.[Confirmation date] is not null



