const { assert } = require('console');

var wr = require('../../core/target/js-2.13/weapon-regex-fastopt')
var mocha = require('assert');
describe('Weapon regeX', function() {
  describe('#mutate()', function() {
    
    it('Can mutate without options', function() {
        mutants = wr.mutate('^a');
        mocha.strictEqual(mutants.length, 2);
    });
    
    it('Can mutate with only mutators as option', function() {
        mutants = wr.mutate('^a', {mutators: Array.from(wr.mutators.values())});
        mocha.strictEqual(mutants.length, 2);
    });
    
    it('Can mutate with only levels as option', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});
        mocha.strictEqual(mutants.length, 1);
    });
    
    it('Can mutate with both levels and mutators as options', function() {
        mutants = wr.mutate('^a', {mutators: Array.from(wr.mutators.values()), mutationLevels: [1]});
        mocha.strictEqual(mutants.length, 1);
    });
  });
  
  describe('Mutant', function() {
    
    it('Contains the replacement pattern', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});

        mocha.strictEqual(mutants.length, 1);
        mocha.strictEqual(mutants[0].pattern, 'a');
    });
    
    it('Contains the mutator name', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});

        mocha.strictEqual(mutants.length, 1);
        mocha.strictEqual(mutants[0].name, 'Beginning of line character `^` removal');
    });
    
    it('Contains the location of the mutation', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});

        mocha.strictEqual(mutants.length, 1);
        mocha.strictEqual(mutants[0].location.start.line, 0);
        mocha.strictEqual(mutants[0].location.start.column, 0);
        mocha.strictEqual(mutants[0].location.end.line, 0);
        mocha.strictEqual(mutants[0].location.end.line, 0);
    });
    
    it('Contains the level of the mutator', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});

        mocha.strictEqual(mutants.length, 1);
        mocha.deepStrictEqual(mutants[0].mutationLevels, [1, 2, 3]);
    });
    
    it('Contains the mutator description', function() {
        mutants = wr.mutate('^a', {mutationLevels: [1]});

        mocha.strictEqual(mutants.length, 1);
        mocha.strictEqual(mutants[0].description, 'Remove beginning of line character `^`');
    });
  });
});

