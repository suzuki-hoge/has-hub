# hub-board
create epics and issues.

## index
+ [download](https://github.com/suzuki-hoge/hub-board#download)
+ [sub commands](https://github.com/suzuki-hoge/hub-board#sub-commands)
+ [configure](https://github.com/suzuki-hoge/hub-board#configure)
+ [yaml description](https://github.com/suzuki-hoge/hub-board#yaml-description)

## download
bin path

## sub commands
### desc

### init

### new-workspace

### post

## configure
+ token
+ token
+ owner
+ repository
+ proxy

tree

## yaml description
yaml consists of 3 blocks, `milestone` block, `default-pipeline` block, and `epics` block, like this.

```yaml
milestone:
    new-milestone:
        title   : sprint 1
        start-on: 2019-04-01
        due-on  : 2019-04-12

default-pipeline: sprint backlog

epics:
  - new-epic:
      title: epic title 1
      body : epic body 1

      issues:
        - title: issue title 1

  - existing-epic:
      number: 1

      issues:
        - title: issue title 2

  - no-epic:
      issues:
        - title: issue title 3
```

### `milestone` block
`milestone` block is allowed to either `new-milestone`, `existing-milestone`, or `no-milestone`.

```yaml
milestone:
    # use new-milestone for start new sprint
    new-milestone:
        title   : sprint 1
        start-on: 2019-02-01
        due-on  : 2019-02-06
```

```yaml
milestone:
    # use existing-milestone for started sprint
    existing-milestone:
        title: sprint 1
```

```yaml
milestone:
    # use no-milestone for non sprint
    no-milestone: true
```

this `milestone` block applies to all epics and issues in yaml.
so 1 yaml represents 1 milestone ( = 1 sprint ).

### `default-pipeline` block
this `default-pipeline` block also applies to all epics and issues in yaml.
but unlike `milestone` block, you can over write at `epics` block and `issues` sub block.
this block prevents "all epics and issues placed on the leftmost".

```yaml
default-pipeline: sprint backlog
```

### `epics` block
`epics` block is allowed to multiple either `new-epic`, `existing-epic`, or `no-epic`.
`existing-epic` and `no-epic` must contain `issues`.

```yaml
epics:
  # 1 issue create, and link to new epic
  - new-epic:
      title: epic title 1

      issues:
        - title: issue title 1

  # create epic only
  - new-epic:
      title: epic title 2

  # 1 issue create, and link to epic ( #1 )
  - existing-epic:
      number: 1

      issues:
        - title: issue title 2

  # 2 issues create, and no epic linking
  - no-epic:
      issues:
        - title: issue title 3
        - title: issue title 4
```

#### `issues` sub block
`issues` sub block is allowed to following attributes.

attribute | type          | required     | default         
:--       | :--           | :--          | :--             
title     | string        | required     | -               
body      | string        | not required | empty           
labels    | [string]      | not required | no labels       
assignees | [string]      | not required | no assignees    
pipeline  | string        | not required | default-pipeline
estimate  | int or double | not required | 0               

```yaml
      issues:
        # full attributes
        - title    : issue title 1
          body     : issue body 1
          labels   : [setup]
          assignees: [suzuki-hoge]
          pipeline : sprint backlog
          estimate : 3

        # empty attributes ( use default )
        - title    : issue title 2
          body     :
          labels   : []
          assignees: []
          pipeline :
          estimate :

        # minimum attributes ( use default )
        - title    : issue title 3

        # multi lines body
        - title    : issue title 4
          body     : |
            # issue body 4
            + [ ] task 1
            + [ ] task 2
          estimate : 0.5
```

also, `new-epic` is allowed same attributes too.

```yaml
epics:
  - new-epic:
      title    : epic title 1
      body     : epic body 1
      labels   : [dev]
      assignees: [suzuki-hoge]
      pipeline : backlog
      estimate : 5
```
