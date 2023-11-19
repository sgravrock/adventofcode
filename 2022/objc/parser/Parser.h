//
//  parser.h
//  parser
//
//  Created by Stephen Gravrock on 11/18/23.
//

#import <Foundation/Foundation.h>

@interface Parser : NSObject

//+ (Parser *) repeating:(Parser *)elemParser;

+ (Parser *) forRegex:(NSString *)pattern into:(Class)resultClass;

- (id) parse:(NSString *)input;

@end
