# command
## Token
```
gtoken='xxx'; ztoken='yyy';
```

## Repository
### Refer
```
curl -sS -H "Authorization: token $gtoken" https://api.github.com/repos/suzuki-hoge/has-hub-workspace | jq .id
131509978
```

## Milestone
### Refer
```
curl -sS -H "Authorization: token $gtoken" 'https://api.github.com/repos/suzuki-hoge/has-hub-workspace/milestones?state=all' | jq -c '.[] | [.number, .title, .due_on]'
[1,"sprint 1",null]
[2,"sprint 2","2018-05-31T07:00:00Z"]
```

```
curl -sS -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/milestones/2/start_date | jq .start_date
"2018-05-23T03:00:00.000Z"
```

### Create
```
curl -sS -H "Authorization: token $gtoken" -X POST https://api.github.com/repos/suzuki-hoge/has-hub-workspace/milestones -d '{"title": "sprint 3", "due_on": "2018-05-01T23:59:59Z"}' | jq -c '[.number, .title, .due_on]'
[3,"sprint 3","2018-05-01T07:00:00Z"]
```

```
curl -sS -H 'content-type:application/json' -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/milestones/1/start_date -X POST -d '{"start_date": "2018-05-01T03:00:00.000Z"}' | jq .start_date
"2018-05-01T03:00:00.000Z"
```

## Label
### Refer
```
curl -sS -H "Authorization: token $gtoken" 'https://api.github.com/repos/suzuki-hoge/has-hub-workspace/labels' | jq -c '.[] | .name'     
"label 1"
```

## Collaborator
### Refer
```
curl -sS -H "Authorization: token $gtoken" https://api.github.com/repos/suzuki-hoge/has-hub-workspace/collaborators | jq -c '.[] | .login'
"suzuki-hoge"
```

## Pipeline
### Refer
```
curl -sS -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/board | jq -c '.pipelines | .[] | [.id, .name]'
["5b02c59d2133e10681389873","backlog"]
["5b0577fa2133e1068138aabc","sprint backlog"]
["5b02c59d2133e10681389876","reviewing"]
```

## Issue
### Create
```
curl -sS -H "Authorization: token $gtoken" https://api.github.com/repos/suzuki-hoge/has-hub-workspace/issues -X POST -d '{"title": "create api", "body": "+ [ ] dev", "assignees": ["suzuki-hoge"], "milestone": 1, "labels": ["bugfix"]}' | jq .number
4
```

```
curl -sS -H 'content-type:application/json' -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/epics/4/update_issues -X POST -d '{"add_issues": [{"repo_id": 131509978, "issue_number": 2}]}'
```

```
curl -sS -H 'content-type:application/json' -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/issues/4/moves -X POST -d '{"pipeline_id": "5b0577fa2133e1068138aabc", "position": "top"}'
```

```
curl -sS -H 'content-type:application/json' -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/issues/2/estimate -X PUT -d '{"estimate": 3}' | jq .estimate
3
```

## Epic
### Refer
```
curl -sS -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/epics | jq -c '.epic_issues[] | .issue_number'
5
4
3
```

### Create
```
curl -sS -H 'content-type:application/json' -H "X-Authentication-Token: $ztoken" https://api.zenhub.io/p1/repositories/131509978/issues/4/convert_to_epic -X POST -d '{}'
```
