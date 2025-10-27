We should extend the tree-schemas to specify whether something is an:
- Object
- if so, then also alowing for saying who do you value it going to:

- Object
    - Contributors (who gave/realized this object)
    - Given to:
        - Self
        - Others
        - Collective
        - ...

object
    Given to (Verbs)
        Self
            - contributors (Subjects)
                contributor ID : amount
        Other
        Collective
        ...


// Auto-populate contributor list based
// Auto-weight by contribution amount
// User decides how much they value object:
    being given to self and others.