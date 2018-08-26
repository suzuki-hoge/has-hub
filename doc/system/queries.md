# query
## Token
```
gtoken='xxx'
```

## Curl
```
curl -sS -H 'Content-Type: application/json' -H "Authorization: bearer $gtoken" -X POST -d '{"query": "query { ... }"}' https://api.github.com/graphql
```

## Repository
### Refer
```
query {
  repository(owner:"suzuki-hoge", name:"has-hub-workspace") {
    databaseId
  }
}
```

## Label
### Refer
```
query {
  repository(owner:"suzuki-hoge", name:"has-hub-workspace") {
    labels(last:100) {
      nodes {
        name
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
```

## Collaborator
### Refer
```
query {
  repository(owner:"suzuki-hoge", name:"has-hub-workspace") {
    assignableUsers(last:100) {
      nodes {
        login
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
```

## Milestone
### Refer
```
query {
  repository(owner:"suzuki-hoge", name:"has-hub-workspace") {
    milestones(last:100, states:OPEN) {
      nodes {
        number
        title
        dueOn
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
```

## Epic
### Refer
```
query {
  repository(owner:"suzuki-hoge", name:"has-hub-workspace") {
    issues(first:2, states:OPEN, labels:["Epic"]) {
      nodes {
        title
        number
      }
      pageInfo {
        hasNextPage
        endCursor
      }
    }
  }
}
```
