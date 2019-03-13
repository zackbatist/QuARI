



```mermaid
graph LR;
    
    level2 --> level3
    style level2 fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level3 fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    
    subgraph level2
    level2.LocusType-.->level2
    level2.Locus-.->level2
    level2.Period-.->level2
    level2.Blank-.->level2
    level2.Modification-.->level2
    level2.Quantity-.->level2
    style level2.LocusType fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level2.Locus fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level2.Period fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level2.Blank fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level2.Modification fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level2.Quantity fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph level3
    level3.ArtefactID-.->level3
    level3.Weathering-.->level3
    level3.Patination-.->level3
    level3.RawMaterial-.->level3
    level3.Morphology-.->level3
    style level3.ArtefactID fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level3.Weathering fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level3.Patination fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level3.RawMaterial fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style level3.Morphology fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph attribs
    subgraph periods
    periods.Period-.->periods
    periods.Abbreviation-.->periods
    style periods.Period fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style periods.Abbreviation fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style periods fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph blanks
    blanks.Blank-.->blanks
    blanks.Period-.->blanks
    style blanks.Blank fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style blanks.Period fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style blanks fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph modifications
    modifications.Modification-.->modifications
    modifications.Period-.->modifications
    style modifications.Modification fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style modifications.Period fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style modifications fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph locustypes
    gridcollectionpoints-.->surveycollectionpoints
    transectcollectionpoints-.->surveycollectionpoints
    grabsamples-.->surveycollectionpoints
    contexts-.->allloci
    surveycollectionpoints-.->allloci
    xfinds-.->allloci
    style allloci fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style contexts fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style xfinds fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style gridcollectionpoints fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style transectcollectionpoints fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style grabsamples fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style surveycollectionpoints fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    end

    subgraph r objects
    allloci-->AllLoci
    level2-->Level2
    level3-->Level3
    periods-->Periods
    blanks-->Blanks
    modifications-->Modifications
    ActivityLog
    Photos
    Illustrations
    end
    
    AllLoci-.->LocusType
    
    activitylog-->ActivityLog
    style activitylog fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    
    subgraph photos
    photos-->Photos
    photos.PhotoID-.->photos
    photos.Filename-.->photos
    photos.ArtefactID-.->photos
    style photos.PhotoID fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style photos.Filename fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style photos.ArtefactID fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style photos fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    subgraph illustrations
    illustrations-->Illustrations
    illustrations.DrawingID-.->illustrations
    illustrations.ArtefactID-.->illustrations
    style illustrations.DrawingID fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style illustrations.ArtefactID fill:#ccf,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    style illustrations fill:#aef,stroke:#f66,stroke-width:2px,stroke-dasharray: 5, 5
    end
    
    illustrations.ArtefactID-->level3.ArtefactID
    photos.ArtefactID-->level3.ArtefactID



```