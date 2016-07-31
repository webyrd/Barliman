//
//  Extensions.swift
//  Barliman
//
//  Created by Adam Nemecek on 7/31/16.
//  Copyright © 2016 William E. Byrd. All rights reserved.
//

import Foundation

extension SequenceType {
    func find(@noescape pred: Generator.Element -> Bool) -> Generator.Element? {
        for e in self where pred(e) {
            return e
        }
        return nil
    }
}
